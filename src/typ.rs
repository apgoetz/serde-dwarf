use std::cmp;
use std::collections::{BTreeMap, HashMap, hash_map};
use std::fmt;
use std::hash;
use std::ops::Index;
use std::convert::TryFrom;

// how to handle recursive types?  the type variable needs to be able
// to serialize something if it is behind a vec, or a reference serde
// gets around this by using traits, the recursive type can call its
// own serialize impl if it find a reference to it in the type
// definition.
//
// We cant do that with the current implementation because it uses
// concrete structs.
//
// one solution would be to use some kind of type lookup. Have a field
// of a type be considered a "reference" and then looking up the type
// in a centralized "dictionary" of types. This requires the type to
// have a back reference to the dictionary it was created from, since
// types can be related to each other.
//
//
// consider the classic case of serializing a linked list:
//
// struct List(Option<Box<List>>)
//
// in this case we need to refer back to the original type when we deserialize the type.
//
// solution: use type lookup graph
//
// what about circularly referenced data types:
//
// struct A(Option<Box<B>>)
// struct B(Option<Box<A>>)
//
// let foo = A(Some(Box::new(B(Some(Box::new(A(None)))))))

type VariantIndex = u32;
pub trait BuilderKey: fmt::Debug + Clone + Send + Sync + cmp::Eq + hash::Hash {}

impl<'a, T> BuilderKey for T
where
    T: 'a + fmt::Debug + Clone + Send + Sync + cmp::Eq + hash::Hash,
    &'a T: hash::Hash,
{
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variant<K: fmt::Debug + Clone> {
    Unit(String),
    NewType(String, K),
    Tuple(String, Vec<K>),
    Struct(String, BTreeMap<String, K>),
}

impl<K, T> TypeDisplay<K, T> for Variant<K>
where
    K: BuilderKey,
    T: Index<K, Output = TypeVariant<K>>,
{
    fn tfmt(&self, f: &mut fmt::Formatter, t: &T) -> Result<(), fmt::Error> {
        match self {
            Variant::Unit(name) => write!(f, "{}", name),
            Variant::NewType(name, typ) => {
                write!(f, "{}({})", name, print_name(typ, t))
            }
            Variant::Tuple(name, elems) => {
                write!(f, "{}(", name)?;
                let elems = elems
                    .iter()
                    .map(|e| print_name(e, t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{})", elems)
            }
            Variant::Struct(name, fields) => {
                write!(f, "{} {{ ", name)?;
                let fields = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, print_name(v, t)))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} }},", fields)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrimitiveType {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Char,
    String,
    ByteArray,
    Unit,
}

impl TryFrom<&str> for PrimitiveType {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "bool" => PrimitiveType::Bool,
            "i8" => PrimitiveType::I8, 
            "i16" => PrimitiveType::I16,
            "i32" => PrimitiveType::I32,
            "i64" => PrimitiveType::I64,
            "isize" => PrimitiveType::I64,
            "i128" => PrimitiveType::I128,
            "u8" => PrimitiveType::U8,
            "u16" => PrimitiveType::U16,
            "u32" => PrimitiveType::U32,
            "u64" => PrimitiveType::U64,
            "usize" => PrimitiveType::U64,
            "u128" => PrimitiveType::U128,
            "f32" => PrimitiveType::F32,
            "f64" => PrimitiveType::F64,
            "char" => PrimitiveType::Char,
            "[u8]" => PrimitiveType::ByteArray,
            "()" => PrimitiveType::Unit,
            _ => return Err("value is not a basetype")
        })
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::Bool => f.write_str("bool"),
            PrimitiveType::I8 => f.write_str("i8"),
            PrimitiveType::I16 => f.write_str("i16"),
            PrimitiveType::I32 => f.write_str("i32"),
            PrimitiveType::I64 => f.write_str("i64"),
            PrimitiveType::I128 => f.write_str("i128"),
            PrimitiveType::U8 => f.write_str("u8"),
            PrimitiveType::U16 => f.write_str("u16"),
            PrimitiveType::U32 => f.write_str("u32"),
            PrimitiveType::U64 => f.write_str("u64"),
            PrimitiveType::U128 => f.write_str("u128"),
            PrimitiveType::F32 => f.write_str("f32"),
            PrimitiveType::F64 => f.write_str("f64"),
            PrimitiveType::Char => f.write_str("char"),
            PrimitiveType::String => f.write_str("String"),
            PrimitiveType::ByteArray => f.write_str("[u8]"),
            PrimitiveType::Unit => f.write_str("()"),
        }
    }
}

pub type TypeKey = usize;

/// Represents a data type described in the debug info
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub(crate) parts: Vec<TypeVariant<TypeKey>>,
    pub(crate) root: TypeKey,
}




impl<K, T> TypeDisplay<K, T> for K
where
    K: BuilderKey,
    T: Index<K, Output = TypeVariant<K>>,
{
    fn tfmt(&self, f: &mut fmt::Formatter, t: &T) -> Result<(), fmt::Error> {
        match t.index(self.clone()) {
            TypeVariant::UnitStruct(name) => write!(f, "struct {}", name),
            TypeVariant::Enum(name, vars) => {
                let vars = vars
                    .iter()
                    .map(|v| v.type_to_string(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "enum {} {{ {} }}", name, vars)
            }
            TypeVariant::NewType(name, typ) => {
                write!(f, "struct {}({})", name, typ.type_to_string(t))
            }
            TypeVariant::TupleStruct(name, types) => {
                let types = types
                    .iter()
                    .map(|subtype| subtype.type_to_string(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "struct {}({})", name, types)
            }
            TypeVariant::Struct(name, fields) => {
                let fields = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, print_name(v, t)))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "struct {} {{ {} }}", name, fields)
            }
            _ => f.write_str(&print_name(self, t)),
        }
    }
}



// represents a data type of a symbol. The data type is always
// monomorphized, because we are looking at compiled code, so types
// cannot be generic.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeVariant<K: fmt::Debug + Clone> {
    Primitive(PrimitiveType),
    Option(K),
    UnitStruct(String),
    Enum(String, Vec<Variant<K>>),
    NewType(String, K),
    Seq(K, Option<usize>),
    Tuple(Vec<K>),
    TupleStruct(String, Vec<K>),
    Map(K, K, Option<usize>),
    Struct(String, BTreeMap<String, K>),
}

impl fmt::Display for TypeVariant<String> {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!();
    }
}

pub struct Iter<'a, K: 'a>(hash_map::Keys<'a, K, TypeVariant<K>>)
    where
    K: BuilderKey,
    K: 'a;


impl<'a, K> Iterator for Iter<'a, K>
    where
    K: BuilderKey
{
    type Item = &'a K;

    #[inline]
    fn next(&mut self) -> Option<&'a K> {
        self.0.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}




pub struct TypeBuilder<K: Clone + fmt::Debug> {
    types: HashMap<K, TypeVariant<K>>,
}

impl<K: BuilderKey> TypeBuilder<K> {
    pub fn new() -> Self {
        TypeBuilder {
            types: HashMap::new(),
        }
    }

    pub fn iter(&self) -> Iter<K>{
        Iter(self.types.keys())
    }
    
    
    // consume another type builder and add its types to ours
    pub fn extend(&mut self, mut other:  TypeBuilder<K>) {
        self.types.extend(other.types.drain());
    }
    
    fn convert_variant<'a>(
        &'a self,
        typ: &'a Variant<K>,
        partial_type: &mut Vec<TypeVariant<TypeKey>>,
        visited: &mut HashMap<&'a K, TypeKey>,
    ) -> Option<Variant<TypeKey>> {
        let v = match typ {
            Variant::Unit(name) => Variant::Unit(String::from(name)),
            Variant::NewType(name, k) => {
                let inner_id = self.get_or_add(k, partial_type, visited)?;
                Variant::NewType(String::from(name), inner_id)
            }
            Variant::Tuple(name, keys) => {
                let orig_len = keys.len();
                let keys = keys
                    .iter()
                    .filter_map(|k| self.get_or_add(k, partial_type, visited))
                    .collect::<Vec<_>>();
                if orig_len != keys.len() {
                    return None;
                }
                Variant::Tuple(String::from(name), keys)
            }
            Variant::Struct(name, fields) => {
                let orig_len = fields.len();
                let fields = fields
                    .iter()
                    .filter_map(|(f, k)| {
                        if let Some(k) = self.get_or_add(k, partial_type, visited) {
                            Some((f.clone(), k))
                        } else {
                            None
                        }
                    })
                    .collect::<BTreeMap<_, _>>();
                if orig_len != fields.len() {
                    return None;
                }
                Variant::Struct(String::from(name), fields)
            }
        };
        Some(v)
    }

    // gets the index of a type in the vec, or adds it if it doesnt exist
    // this is how we build types
    fn get_or_add<'a>(
        &'a self,
        typ: &'a K,
        partial_type: &mut Vec<TypeVariant<TypeKey>>,
        visited: &mut HashMap<&'a K, TypeKey>,
    ) -> Option<TypeKey> {
        // if we have already seen this data type, return its index
        if let Some(index) = visited.get(typ) {
            return Some(*index);
        }
        // otherwise, we need to add it
        // TODO, fix panic with error code
        let variant = self.types.get(typ)?;

        // we dont know if this is a recursive type, so we
        // need to insert this type before we reference the
        // inner type.
        let idx = partial_type.len();
        visited.insert(typ, idx);
        partial_type.push(TypeVariant::Primitive(PrimitiveType::Unit));

        partial_type[idx] = match variant {
            // if we are a primitive, we just add ourselves
            TypeVariant::Primitive(p) => TypeVariant::Primitive(*p),

            // if we are an option, we need to make sure our
            // underlying type is added and then add the option type
            TypeVariant::Option(tref) => {
                let inner_ref = self.get_or_add(&tref, partial_type, visited)?;
                TypeVariant::Option(inner_ref)
            }
            TypeVariant::UnitStruct(name) => TypeVariant::UnitStruct(name.clone()),
            TypeVariant::Enum(name, variants) => {
                let orig_len = variants.len();
                let variants = variants
                    .iter()
                    .filter_map(|v| self.convert_variant(v, partial_type, visited))
                    .collect::<Vec<_>>();
                if orig_len != variants.len() {
                    return None;
                }
                TypeVariant::Enum(String::from(name), variants)
            }
            TypeVariant::NewType(name, tref) => {
                let inner_ref = self.get_or_add(&tref, partial_type, visited)?;
                // update our type with the real variant
                TypeVariant::NewType(String::from(name), inner_ref)
            }
            TypeVariant::Seq(typ, len) => {
                let inner_ref = self.get_or_add(&typ, partial_type, visited)?;
                TypeVariant::Seq(inner_ref, *len)
            }
            TypeVariant::Tuple(types) => {
                let orig_len = types.len();
                let types = types
                    .iter()
                    .filter_map(|k| self.get_or_add(k, partial_type, visited))
                    .collect::<Vec<_>>();
                if orig_len != types.len() {
                    return None;
                }
                TypeVariant::Tuple(types)
            }
            TypeVariant::TupleStruct(name, types) => {
                let orig_len = types.len();
                let types = types
                    .iter()
                    .filter_map(|k| self.get_or_add(k, partial_type, visited))
                    .collect::<Vec<_>>();
                if orig_len != types.len() {
                    return None;
                }
                TypeVariant::TupleStruct(name.clone(), types)
            }
            TypeVariant::Map(key, val, len) => {
                let key = self.get_or_add(key, partial_type, visited)?;
                let val = self.get_or_add(val, partial_type, visited)?;
                TypeVariant::Map(key, val, *len)
            }
            TypeVariant::Struct(name, fields) => {
                let orig_len = fields.len();
                let fields = fields
                    .iter()
                    .filter_map(|(f, k)| {
                        if let Some(k) = self.get_or_add(k, partial_type, visited) {
                            Some((f.clone(), k))
                        } else {
                            None
                        }
                    })
                    .collect::<BTreeMap<_, _>>();
                if orig_len != fields.len() {
                    return None;
                }
                TypeVariant::Struct(name.clone(), fields)
            }
        };
        Some(idx)
    }

    
    
    // build a type from what is present here
    pub fn build(&self, root: K) -> Option<Type> {
        let mut parts = Vec::new();
        let mut visited = HashMap::new();
        let root = self.get_or_add(&root, &mut parts, &mut visited)?;
        Some(Type { parts, root })
    }

    pub fn add_prim<'a>(&'a mut self, k: K, p: PrimitiveType) -> &'a TypeBuilder<K> {        
        self.types.insert(k, TypeVariant::Primitive(p));
        self
    }

    pub fn add_option<'a>(&'a mut self, k: K, t: K) -> &'a TypeBuilder<K> {
        self.types.insert(k, TypeVariant::Option(t));
        self
    }

    pub fn add_unit_struct<'a>(&'a mut self, k: K, name: &str) -> &'a TypeBuilder<K> {
        if ! self.types.contains_key(&k) {
            self.types
                .insert(k, TypeVariant::UnitStruct(String::from(name)));
        }
        self
    }

    pub fn add_enum<'a>(
        &'a mut self,
        k: K,
        name: &str,
        variants: &Vec<Variant<K>>,
    ) -> &'a TypeBuilder<K> {
        if ! self.types.contains_key(&k) {
            self.types
                .insert(k, TypeVariant::Enum(String::from(name), variants.clone()));
        }
        self
    }

    pub fn add_newtype<'a>(&'a mut self, k: K, name: &str, t: K) -> &'a TypeBuilder<K> {
        if ! self.types.contains_key(&k) {
            self.types
                .insert(k, TypeVariant::NewType(String::from(name), t));
        }
        self
    }

    pub fn add_seq<'a>(&'a mut self, key: K, typ: K, len: Option<usize>) -> &'a TypeBuilder<K> {
        self.types.insert(key, TypeVariant::Seq(typ, len));
        self
    }

    pub fn add_tuple<'a>(&'a mut self, key: K, types: &[K]) -> &'a TypeBuilder<K> {
        if ! self.types.contains_key(&key) {
            let types = Vec::from(types);
            self.types.insert(key, TypeVariant::Tuple(types));
        }
        self
    }

    pub fn add_tuple_struct<'a>(&'a mut self, key: K, name: &str, types: &[K]) -> &'a TypeBuilder<K> {
        if ! self.types.contains_key(&key) {
            let types = Vec::from(types);
            self.types
                .insert(key, TypeVariant::TupleStruct(String::from(name), types));
        }
        self
    }

    pub fn add_map<'a>(&'a mut self, key: K, k: K, v: K, len: Option<usize>) -> &'a TypeBuilder<K> {
        self.types.insert(key, TypeVariant::Map(k, v, len));
        self
    }

    pub fn add_struct<'a>(
        &'a mut self,
        key: K,
        name: &str,
        fields: &BTreeMap<String, K>,
    ) -> &'a TypeBuilder<K> {
        if ! self.types.contains_key(&key) {
            self.types
                .insert(key, TypeVariant::Struct(String::from(name), fields.clone()));
        }
        self
    }

    pub fn contains_key(&self, k: K) -> bool {
        self.types.contains_key(&k)
    }
    
    // escape hatch to add a raw type. Do we need this???
    fn add_type<'a>(&'a mut self, k: K, t: TypeVariant<K>) -> &'a TypeBuilder<K> {
        self.types.insert(k, t);
        self
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.root.tfmt(f, &self.parts)
    }
}

// helper trait for various things to format themselves
trait TypeDisplay<I, T: Index<I>> {
    fn tfmt(&self, f: &mut fmt::Formatter<'_>, t: &T) -> fmt::Result;

    // this hack taken from https://github.com/rust-lang/rust/issues/46591#issue-280605901
    fn type_to_string(&self, t: &T) -> String
    where
        Self: Sized,
    {
        let fmt = {
            struct ManualDisplay<'a, I, T: Index<I>>(&'a dyn TypeDisplay<I, T>, &'a T);

            impl<'a, I, T: Index<I>> fmt::Display for ManualDisplay<'a, I, T> {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    self.0.tfmt(f, self.1)
                }
            }

            ManualDisplay(self, t)
        };
        format!("{}", fmt)
    }
}

// just print the name of the type
fn print_name<K, T>(key: &K, t: &T) -> String
where
    K: BuilderKey,
    T: Index<K, Output = TypeVariant<K>>,
{
    match t.index(key.clone()) {
        TypeVariant::Primitive(p) => p.to_string(),
        TypeVariant::Option(opt) => format!("Option<{}>", print_name(opt, t)),
        TypeVariant::UnitStruct(name) => name.to_string(),
        TypeVariant::Enum(name, _) => name.to_string(),
        TypeVariant::NewType(name, _) => name.to_string(),
        TypeVariant::Seq(typ, len) => format!(
            "[ {} ; {} ]",
            print_name(typ, t),
            len.map_or("?".to_string(), |l| l.to_string())
        ),
        TypeVariant::Tuple(types) => {
            let types = types
                .iter()
                .map(|subtype| print_name(subtype, t))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", types)
        }
        TypeVariant::TupleStruct(name, _) => name.to_string(),
        TypeVariant::Map(key, val, len) => format!(
            "{{ {}: {} ; {} }}",
            print_name(key, t),
            print_name(val, t),
            len.map_or("?".to_string(), |l| l.to_string())
        ),
        TypeVariant::Struct(name, _) => name.to_string(),
    }
}

#[cfg(test)]
mod tests {

    macro_rules! assert_fmt {
        ($builder:ident, $($key:literal => $rendered:literal),*) => {
            $(
                let val = $builder.build($key).unwrap();
                assert_eq!(val.to_string(), $rendered);
            )*
        }
    }

    use crate::typ::*;
    //    use std::env;
    //    use crate::DebugInfo;
    #[test]
    fn enum_variants() {
        // assert_eq!("Foo,", Variant::Unit("Foo".to_string()).to_string(&Type::new_prim(PrimitiveType::Unit)));
    }

    #[test]
    fn basic_types() {
        let mut builder = TypeBuilder::new();
        builder.add_prim("unit", PrimitiveType::Unit);
        builder.add_prim("u8", PrimitiveType::U8);
        builder.add_option("option_unit", "unit");

        builder.add_unit_struct("unit_struct", "Foo");
        builder.add_option("option_foo", "unit_struct");

        let tuple_var = Variant::Tuple("Baz".to_string(), vec!["unit", "unit_struct"]);
        let enum_variants = vec![
            Variant::Unit("Foo".to_string()),
            Variant::NewType("Bar".to_string(), "u8"),
            tuple_var,
        ];
        builder.add_enum("enum", "MyEnum", &enum_variants);

        builder.add_seq("seq", "enum", Some(5));
        builder.add_seq("seq?", "unit_struct", None);

        builder.add_tuple("t1", &["unit"]);
        builder.add_tuple("t2", &["enum", "enum"]);

        builder.add_tuple_struct("rgb", "RGB", &["u8", "u8", "u8"]);

        builder.add_map("map2", "u8", "enum", Some(2));
        builder.add_map("map?", "u8", "enum", None);

        let mut fields = BTreeMap::new();
        fields.insert(String::from("id"), "u8");
        fields.insert(String::from("vals"), "seq?");
        builder.add_struct("struct", "MyStruct", &fields);

        // unit struct
        assert_fmt!(builder,
                    "unit_struct" => "struct Foo",
                    "unit" => "()",
                    "option_unit" => "Option<()>",
                    "option_foo" => "Option<Foo>",
                    "enum" => "enum MyEnum { Foo, Bar(u8), Baz((), Foo) }",
                    "seq" => "[ MyEnum ; 5 ]",
                    "seq?" => "[ Foo ; ? ]",
                    "t1" => "(())",
                    "t2" => "(MyEnum, MyEnum)",
                    "rgb" => "struct RGB(u8, u8, u8)",
                    "map2" => "{ u8: MyEnum ; 2 }",
                    "map?" => "{ u8: MyEnum ; ? }",
                    "struct" => "struct MyStruct { id: u8, vals: [ Foo ; ? ] }"
        );
    }

    #[test]
    fn recursive_newtype() {
        // build a recursive type like struct Foo(Option<Box<Foo>>);
        let mut builder = TypeBuilder::new();
        builder.add_newtype("mytype", "Foo", "opt");
        builder.add_option("opt", "mytype");
        assert_fmt!(builder, "mytype" => "struct Foo(Option<Foo>)");
    }

    #[test]
    fn mutually_recursive() {
        // build a  set of 2 recursive types like:
        // struct A(Option<Box<B>>);
        // struct B(Option<Box<A>>);
        let mut builder = TypeBuilder::new();
        builder.add_newtype("a", "A", "optb");
        builder.add_newtype("b", "B", "opta");
        builder.add_option("opta", "a");
        builder.add_option("optb", "b");

        assert_fmt!(builder, "a" => "struct A(Option<B>)");
    }

    #[test]
    fn missing_type() {
        let mut builder = TypeBuilder::new();
        builder.add_option("opta", "a");
        assert!(builder.build("a").is_none());
    }
}
