use std::{
    collections::BTreeMap,
    env::current_dir,
    fs::{self, read_to_string, File, OpenOptions},
    io::{BufWriter, Write},
    path::PathBuf,
};

use combu::{
    action_result, command::presets::func::help_tablize_with_alias_dedup, copyright, crate_authors,
    crate_license, crate_version, done, flags, license, preset_help_command, vector, Command,
    Context, Flag,
};
use nibi::app::{
    category::{insert_descendant_to_category_list, Category},
    tag::Tag,
};
use percent_encoding::percent_decode_str;
use ron::ser::PrettyConfig;
use serde::{de, Deserialize, Deserializer, Serialize};
use serde_json::{from_value, Value};

fn main() {
    let _ = Command::with_all_field(
        "rutsubo".to_owned(),
        None,
        crate_authors!().to_owned(),
        copyright!(2022, suquiya),
        license!(crate_license!().to_owned(),file_path=>"../LICENSE"),
        Some(crate_authors!().to_owned()),
        "rutsubo [subcommand]".to_owned(),
        flags!(),
        flags!(help, version, license, authors, copyright),
        vector![],
        crate_version!().to_owned(),
        vector![sub_help(), conv()],
    )
    .run_with_auto_arg_collect();
}

pub fn sub_help() -> Command {
    preset_help_command!(help_tablize_with_alias_dedup)
}

pub fn conv() -> Command {
    Command::with_all_field(
        "convert".to_owned(),
        Some(convert),
        crate_authors!().to_owned(),
        copyright!(2022, suquiya),
        license!(crate_license!().to_owned(),file_path=>"../LICENSE"),
        Some("convert wordpress post data to nibi post data.".to_owned()),
        "convert [from_path] [to_path]".to_owned(),
        flags!(),
        flags!(),
        vector!["conv".to_owned()],
        crate_version!().to_owned(),
        vector!(),
    )
}

pub fn convert(_cmd: Command, ctx: Context) -> action_result!() {
    println!("exe: {}", ctx.exe_path());
    let current_dir = current_dir().unwrap();
    println!("current_dir: {}", current_dir.display());
    let src = get_path_from_arg(&ctx, &current_dir, 0, "src");
    let dest = get_path_from_arg(&ctx, &current_dir, 1, "dest");
    println!("src: {}, dest: {}", src.display(), dest.display());
    convert_data(&src, &dest);

    done!()
}

#[derive(Debug, Deserialize, Serialize)]
struct WpTermData {
    pub taxonomy_type: WpTaxonomyType,
    pub record: Value,
}
#[derive(Debug, Deserialize, Serialize)]
enum WpTaxonomyType {
    Category,
    PostTag,
}

struct IdValueUnit {
    pub id: u64,
    _value: Value,
}

impl IdValueUnit {
    fn new(id: u64, value: Value) -> Self {
        IdValueUnit { id, _value: value }
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct WpTermValue {
    #[serde(deserialize_with = "deserialize_usize")]
    pub term_id: usize,
    pub name: String,
    pub description: String,
    pub slug: String,
    #[serde(deserialize_with = "deserialize_usize")]
    pub parent: usize,
}

fn deserialize_usize<'de, D>(deserializer: D) -> Result<usize, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    Ok(s.parse().unwrap())
}

fn convert_data(src: &PathBuf, dest: &PathBuf) {
    //let date_format = format_description!("[year]-[month]-[day] [hour]:[minute]:[second]");

    let (mut term_taxonomy, taxonomy_term) =
        get_table_data_as_value_array(src, "term_taxonomy.json")
            .into_iter()
            .fold((BTreeMap::new(), BTreeMap::new()), |mut map, v| {
                let taxonomy = v["taxonomy"].as_str().unwrap();
                if taxonomy == "category" {
                    let term_id: u64 = v["term_id"].as_str().unwrap().parse().unwrap();
                    let term_taxonomy_id: u64 =
                        v["term_taxonomy_id"].as_str().unwrap().parse().unwrap();
                    map.1.insert(term_taxonomy_id, term_id);
                    map.0.insert(term_id, (WpTaxonomyType::Category, v));
                } else if taxonomy == "post_tag" {
                    let term_id: u64 = v["term_id"].as_str().unwrap().parse().unwrap();
                    let term_taxonomy_id: u64 =
                        v["term_taxonomy_id"].as_str().unwrap().parse().unwrap();
                    map.1.insert(term_taxonomy_id, term_id);
                    map.0.insert(term_id, (WpTaxonomyType::PostTag, v));
                }
                map
            });

    let terms = get_table_data_as_value_array(src, "terms.json")
        .into_iter()
        .fold(BTreeMap::new(), |mut map, mut v| {
            let term_id: u64 = v["term_id"].as_str().unwrap().parse().unwrap();
            match term_taxonomy.remove(&term_id) {
                Some(mut tt_value) => {
                    v.as_object_mut()
                        .unwrap()
                        .append(tt_value.1.as_object_mut().unwrap());
                    map.insert(
                        term_id,
                        WpTermData {
                            taxonomy_type: tt_value.0,
                            record: v,
                        },
                    );
                }
                None => {}
            }
            map
        });

    let term_relation = get_table_data_as_value_array(src, "term_relationships.json")
        .into_iter()
        .fold(BTreeMap::<u64, Vec<IdValueUnit>>::new(), |mut map, v| {
            let object_id: u64 = v["object_id"].as_str().unwrap().parse().unwrap();
            let term_taxonomy_id: u64 = v["term_taxonomy_id"].as_str().unwrap().parse().unwrap();
            match taxonomy_term.get(&term_taxonomy_id) {
                Some(term_id) => {
                    let _v = IdValueUnit::new(*term_id, v);
                    match map.get_mut(&object_id) {
                        Some(ct) => ct.push(_v),
                        None => {
                            map.insert(object_id, vec![_v]);
                        }
                    }
                }
                None => {}
            }
            map
        });

    let data = {
        let mut d: Vec<WpPostData> =
            serde_json::from_value(get_table_data_as_value(src, "posts.json")).unwrap();
        d = d
            .into_iter()
            .filter(|d| {
                let t = d.post_type.as_str();
                t == "post" || t == "page"
            })
            .collect();
        d.sort_by_key(|v| v.id);
        d
    };

    let ingot_dest_dir = {
        let mut d = dest.clone();
        d.push("ingot");
        d
    };

    if ingot_dest_dir.is_dir() {
        fs::remove_dir_all(&ingot_dest_dir).expect("cannot delete ingot_dest_dir.");
    }
    fs::create_dir(&ingot_dest_dir).expect("cannot create ingot_dest_dir.");
    let cat_prefix = b"categories:";
    let tag_prefix = b"tags:";
    let type_prefix = b"type:";
    let status_prefix = b"status:";
    let pd_prefix = b"published:";
    let ud_prefix = b"updated:";
    let urln_prefix = b"post_url_name:";
    let ingot_id_prefix = b"post_id:";
    let sep = b",";
    let new_line = b"\r\n";
    let mut file_names: BTreeMap<String, usize> = BTreeMap::new();
    let mut ingot_id: usize = 0;
    for d in data {
        let id = d.id;
        let mut categories: Vec<&str> = Vec::new();
        let mut tags: Vec<&str> = Vec::new();
        let output_ext = "ingot";
        match term_relation.get(&id) {
            Some(relations) => {
                for r in relations {
                    let term_id = r.id;
                    let term = terms.get(&term_id).unwrap();
                    match term.taxonomy_type {
                        WpTaxonomyType::Category => {
                            categories.push(term.record["name"].as_str().unwrap());
                        }
                        WpTaxonomyType::PostTag => {
                            tags.push(term.record["name"].as_str().unwrap());
                        }
                    }
                }
            }
            None => {}
        }
        let (dest_file_path, post_name) = {
            let mut f = ingot_dest_dir.clone();
            let pn;
            if d.post_name.is_empty() {
                let mut f_name = if d.post_title.is_empty() {
                    d.post_type.clone() + "-" + &d.post_status + "-" + &d.id.to_string()
                } else {
                    let post_title = &d.post_title;
                    post_title.replace("/", "or")
                };
                if let Some(num) = file_names.get_mut(&f_name) {
                    *num = *num + 1;
                    f_name = f_name + "-" + &num.to_string();
                    if file_names.contains_key(&f_name) {
                        panic!("post name and title is too confused");
                    }
                }
                pn = f_name.clone();
                f.push(&f_name);
                file_names.insert(f_name, 1);
            } else {
                let post_name = percent_decode_str(&d.post_name).decode_utf8().unwrap();
                match file_names.get_mut(post_name.as_ref()) {
                    Some(num) => {
                        *num = *num + 1;
                        let f_name = post_name.to_string() + "-" + &num.to_string();
                        if file_names.contains_key(&f_name) {
                            panic!("post name and title is too confusded");
                        }
                        f.push(&f_name);
                        pn = f_name;
                    }
                    None => {
                        f.push(post_name.as_ref());
                        pn = post_name.to_string();
                    }
                };
            }
            f.set_extension(output_ext);
            (f, pn)
        };
        let dest_file = open_dest_file(dest_file_path);
        let mut bw = BufWriter::new(dest_file);
        // カテゴリー書き出し
        bw.write_all(cat_prefix).unwrap();
        bw.write_all(ron::to_string(&categories).unwrap().as_bytes())
            .unwrap();
        bw.write_all(new_line).unwrap();
        // タグ書き出し
        bw.write_all(tag_prefix).unwrap();
        bw.write_all(ron::to_string(&tags).unwrap().as_bytes())
            .unwrap();
        bw.write_all(&new_line.repeat(2)).unwrap();
        bw.write_all(d.post_title.as_bytes()).unwrap();
        bw.write_all(&new_line.repeat(2)).unwrap();
        bw.write_all(d.post_content.as_bytes()).unwrap();
        bw.write_all(&new_line.repeat(3)).unwrap();
        bw.write_all(type_prefix).unwrap();
        bw.write_all(&d.post_type.as_bytes()).unwrap();
        bw.write_all(sep).unwrap();
        bw.write_all(status_prefix).unwrap();
        bw.write_all(&d.post_status.as_bytes()).unwrap();
        bw.write_all(new_line).unwrap();
        bw.write_all(ud_prefix).unwrap();
        bw.write_all(
            ron::to_string(&vec![&d.post_modified, &d.post_modified_gmt])
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
        bw.write_all(new_line).unwrap();
        bw.write_all(pd_prefix).unwrap();
        bw.write_all(
            ron::to_string(&vec![&d.post_date, &d.post_date_gmt])
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
        bw.write_all(new_line).unwrap();
        bw.write_all(urln_prefix).unwrap();
        bw.write_all(post_name.as_bytes()).unwrap();
        bw.write_all(new_line).unwrap();
        bw.write_all(ingot_id_prefix).unwrap();
        bw.write_all(ingot_id.to_string().as_bytes()).unwrap();
        bw.write_all(new_line).unwrap();
        ingot_id += 1;
        bw.flush().unwrap();
    }

    let (categorys, tags): (Vec<Category>, Vec<Tag>) = terms.into_iter().fold(
        (Vec::new(), Vec::new()),
        |(mut categories, mut tags), term| {
            let (term_id, tv) = term;
            let taxonomy_type = tv.taxonomy_type;
            let val: WpTermValue = from_value(tv.record).unwrap();
            match taxonomy_type {
                WpTaxonomyType::Category => {
                    if val.parent > 0 {
                        let c = Category::new_with_parent(
                            term_id.try_into().unwrap(),
                            val.slug,
                            val.name,
                            val.description,
                            Some(val.parent),
                        );
                        let r = insert_descendant_to_category_list(&mut categories, c);
                        if let Some(r) = r {
                            categories.push(r);
                        }
                    } else {
                        categories.push(Category::new(
                            term_id.try_into().unwrap(),
                            val.slug,
                            val.name,
                            val.description,
                        ));
                    }
                }
                WpTaxonomyType::PostTag => {
                    tags.push(Tag::new(
                        term_id.try_into().unwrap(),
                        val.slug,
                        val.name,
                        val.description,
                    ));
                }
            };
            (categories, tags)
        },
    );
    let categories_path = get_dest_file_path(dest, "categories.ron");
    let mut categories_file = open_dest_file(categories_path);
    categories_file
        .write_all(
            ron::ser::to_string_pretty(&categorys, PrettyConfig::default())
                .unwrap()
                .as_bytes(),
        )
        .unwrap();

    let tags_path = get_dest_file_path(dest, "tags.ron");
    let mut tags_file = open_dest_file(tags_path);
    tags_file
        .write_all(
            ron::ser::to_string_pretty(&tags, PrettyConfig::default())
                .unwrap()
                .as_bytes(),
        )
        .unwrap();
}

fn get_dest_file_path(dest_dir: &PathBuf, file_name: &str) -> PathBuf {
    let mut dest_file_path = dest_dir.clone();
    dest_file_path.push(file_name);
    dest_file_path
}

fn open_dest_file(dest_file_path: PathBuf) -> File {
    OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(dest_file_path)
        .unwrap()
}

fn get_table_data_as_value_array(src_dir: &PathBuf, file_name: &str) -> Vec<Value> {
    let v = get_table_data_as_value(src_dir, file_name);
    match v {
        Value::Array(v) => v,
        _ => {
            panic!("no data found");
        }
    }
}

fn get_table_data_as_value(src_dir: &PathBuf, file_name: &str) -> Value {
    let src_str = read_src_file(src_dir, file_name);
    let mut j: Value = serde_json::from_str(&src_str).unwrap();
    let mut table = j[2].take();
    if table["type"] == "table" {
        println!("detected table name: {}", table["name"]);
        table["data"].take()
    } else {
        panic!("no data found");
    }
}

fn read_src_file(src_dir: &PathBuf, file_name: &str) -> String {
    let fp = {
        let mut fp = src_dir.clone();
        fp.push(file_name);
        fp
    };
    read_to_string(fp).unwrap()
}

fn get_path_from_arg(ctx: &Context, current_dir: &PathBuf, index: usize, default: &str) -> PathBuf {
    match ctx.args.get(index) {
        Some(arg) => {
            let inputted = PathBuf::from(arg);
            if inputted.is_absolute() {
                inputted
            } else {
                let mut path = current_dir.clone();
                path.push(inputted);
                path
            }
        }
        None => {
            let mut path = current_dir.clone();
            path.push(default);
            path
        }
    }
}
#[derive(Debug, Deserialize, Serialize)]
pub struct WpPostData {
    #[serde(rename = "ID", deserialize_with = "deserialize_u64")]
    pub id: u64,
    #[serde(deserialize_with = "deserialize_u64")]
    pub post_author: u64,
    pub post_date: String,
    pub post_date_gmt: String,
    pub post_content: String,
    pub post_title: String,
    pub post_excerpt: String,
    pub post_status: String,
    pub comment_status: String,
    pub ping_status: String,
    pub post_name: String,
    pub to_ping: String,
    pub pinged: String,
    pub post_modified: String,
    pub post_modified_gmt: String,
    pub post_content_filtered: String,
    #[serde(deserialize_with = "deserialize_u64")]
    pub post_parent: u64,
    pub guid: String,
    #[serde(deserialize_with = "deserialize_i32")]
    pub menu_order: i32,
    pub post_type: String,
    pub post_mime_type: String,
    #[serde(deserialize_with = "deserialize_i64")]
    pub comment_count: i64,
}

fn deserialize_u64<'de, D>(deserializer: D) -> Result<u64, D::Error>
where
    D: Deserializer<'de>,
{
    match Value::deserialize(deserializer)? {
        Value::String(s) => {
            let u: u64 = s.parse().unwrap();
            Ok(u)
        }
        Value::Number(num) => Ok(num.as_u64().unwrap()),
        _ => return Err(de::Error::custom("invalid type")),
    }
}

fn deserialize_i64<'de, D>(deserializer: D) -> Result<i64, D::Error>
where
    D: Deserializer<'de>,
{
    match Value::deserialize(deserializer)? {
        Value::String(s) => {
            let i: i64 = s.parse().unwrap();
            Ok(i)
        }
        Value::Number(num) => Ok(num.as_i64().unwrap()),
        _ => return Err(de::Error::custom("invalid type")),
    }
}

fn deserialize_i32<'de, D>(deserializer: D) -> Result<i32, D::Error>
where
    D: Deserializer<'de>,
{
    match Value::deserialize(deserializer)? {
        Value::String(s) => {
            let i: i32 = s.parse().unwrap();
            Ok(i)
        }
        Value::Number(num) => {
            let i = num.as_i64().unwrap();
            Ok(i32::try_from(i).unwrap())
        }
        _ => return Err(de::Error::custom("invalid type")),
    }
}
