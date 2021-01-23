use std::collections::{BTreeMap,HashMap};
use std::fmt;
use std::ops::Index;
#[derive(Debug)]
pub enum Variant<K:fmt::Debug+Clone> {
    Unit(String),
    NewType(String, TypeReference<K>),
    Tuple(String, Vec<TypeReference<K>>),
    Struct(String, BTreeMap<String,TypeReference<K>>)
}

// impl TypeDisplay for Variant<K> {
//     fn tfmt(&self, f: &mut fmt::Formatter<'_>, t: &Type) -> fmt::Result {
//         match self {
//             Variant::Unit(name) => write!(f, "{},", name),
//             Variant::NewType(name, typ) => {
//                 write!(f, "{}({}),", name, typ.name(t))
//             },
//             Variant::Tuple(name,elems) => {
//                 write!(f, "{}(", name)?;
//                 let elems = elems
//                     .iter()
//                     .map(|e|e.name(t))
//                     .collect::<Vec<_>>()
//                     .join(",");
//                 write!(f,"{}),", elems)
//             },
//             Variant::Struct(name, fields) => {
//                 write!(f, "{} {{ ", name)?;
//                 let fields = fields
//                     .iter()
//                     .map(|(k,v)|format!("{}: {}", k, v.name(t)))
//                     .collect::<Vec<_>>()
//                     .join(", ");
//                 write!(f, "{} }},", fields)
//             },
//         }
//     }
// }

#[derive(Debug, Copy, Clone)]
pub enum  PrimitiveType {
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


type TypeKey = usize;

#[derive(Debug, Clone)]
pub struct Type {
    parts: Vec<TypeVariant<TypeKey>>,
    root: TypeReference<TypeKey>,
}


#[derive(Debug, Clone)]
pub struct TypeReference<K:fmt::Debug+Clone>(K);

impl<K: fmt::Debug+Clone+Copy> TypeReference<K> {
    // print the short name of the type
    pub fn name<T>(&self, t: &T) -> String
        where
        T: Index<K,Output = TypeVariant<K>>
    {
        match t.index(self.0) {
            TypeVariant::Primitive(p) => p.to_string(),
            TypeVariant::Option(opt) => format!("Option<{}>", opt.name(t)),
            TypeVariant::UnitStruct(name) => name.to_string(),
            TypeVariant::Enum(name, _) => name.to_string(),
            TypeVariant::NewType(name, _) => name.to_string(),
            TypeVariant::Seq(typ, len) => format!("[ {} ; {} ]",
						  typ.name(t),
						  len.map_or("?".to_string(), |l|l.to_string())),
            TypeVariant::Tuple(types) => {
                let types = types
                    .iter()
                    .map(|subtype| subtype.name(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", types)
            },
            TypeVariant::TupleStruct(name, _) => name.to_string(),
            TypeVariant::Map(key,val,len) => format!("{{ {}: {} ; {} }}",
					             key.name(t),
					             val.name(t),
					             len.map_or("?".to_string(), |l|l.to_string())),
            TypeVariant::Struct(name, _) => name.to_string(),                    
        }
    }
}



impl<K,T> TypeDisplay<K,T> for TypeReference<K>
    where
    K:fmt::Debug+Clone+Copy,
    T:Index<K, Output = TypeVariant<K>>,
{
    fn tfmt(&self, f: &mut fmt::Formatter, t: &T) -> Result<(), fmt::Error> {
        match t.index(self.0) {                    
            TypeVariant::UnitStruct(name) => write!(f,"struct {}", name),
            TypeVariant::Enum(name, vars) => {
                let vars = vars
                    .iter()
                    .map(|v|v.to_string(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "enum {} {{ {} }}", name, vars)
            },
            TypeVariant::NewType(name, typ) => write!(f, "struct {}({})", name, typ.to_string(t)),
            TypeVariant::TupleStruct(name, types) => {
                let types = types
                    .iter()
                    .map(|subtype| subtype.to_string(t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "struct {}({})", name, types)
            },
            TypeVariant::Struct(name, fields) => {
                let fields = fields
                    .iter()
                    .map(|(k,v)| format!("{}: {}", k, v.name(t)))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "struct {} {{ {} }}", name, fields)
            },
            _ => f.write_str(&self.name(t)),
        }
    }
}

// represents a data type of a symbol. The data type is always
// monomorphized, because we are looking at compiled code, so types
// cannot be generic.
#[derive(Debug, Clone)]
pub enum TypeVariant<K:fmt::Debug+Clone> {
    Primitive(PrimitiveType),
    Option(TypeReference<K>),
    UnitStruct(String),
    Enum(String, Vec<TypeReference<K>>),
    NewType(String, TypeReference<K>),
    Seq(TypeReference<K>, Option<usize>),
    Tuple(Vec<TypeReference<K>>),
    TupleStruct(String, Vec<TypeReference<K>>),
    Map(TypeReference<K>, TypeReference<K>, Option<usize>),
    Struct(String, BTreeMap<String,TypeReference<K>>),
}

impl fmt::Display for TypeVariant<String> {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!();
    }
}


struct TypeBuilder {
    types: HashMap<String, TypeVariant<String>>
}

impl TypeBuilder {
    fn new() -> Self {
        TypeBuilder{types: HashMap::new()}
    }

    // gets the index of a type in the vec, or adds it if it doesnt exist
    // this is how we build types
    fn get_or_add<'a>(&'a self,
                      typ: &'a str,
                      partial_type: &mut Vec<TypeVariant<TypeKey>>,
                      visited: &mut HashMap<&'a str, TypeKey>) -> TypeReference<TypeKey> {
        // if we have already seen this data type, return its index
        if let Some(index) = visited.get(typ) {
            return TypeReference(*index);
        }
        // otherwise, we need to add it 
        // TODO, fix panic with error code
        let variant = self.types.get(typ).unwrap(); 
        match variant {
            // if we are an option, we need to make sure our
            // underlying type is added and then add the option type
            TypeVariant::Option(tref) => {
                let inner_ref = self.get_or_add(&tref.0, partial_type, visited);
                let idx = partial_type.len();
                partial_type.push(TypeVariant::Option(inner_ref));
                visited.insert(typ, idx);
                return TypeReference(idx)
            },
            // if we are a primitive, we just add ourselves
            TypeVariant::Primitive(p) => {
                let idx = partial_type.len();
                partial_type.push(TypeVariant::Primitive(*p));
                visited.insert(typ, idx);
                return TypeReference(idx)
            },
            //more work here
            _ => todo!()
        }
    }
    
    // build a type from what is present here
    fn build(&self, root: &str) -> Option<Type> {
        let mut parts = Vec::new();
        let mut visited = HashMap::new();
        let root = self.get_or_add(root, &mut parts, &mut visited);
        Some(Type{parts, root})
    }
    
    fn add_prim<'a>(&'a mut self, p: PrimitiveType) -> &'a TypeBuilder {
        self.types.insert(p.to_string(), TypeVariant::Primitive(p));
        self
    }

    // build a new option type based on an existing type
    fn add_option<'a>(&'a mut self, t: &str) -> &'a TypeBuilder {
        let option = TypeVariant::Option(TypeReference(String::from(t)));
        self.types.insert(option.to_string(), option);
        self
    }
}


impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.root.tfmt(f,&self.parts)
    }
}

#[cfg(test)]
mod tests {

    macro_rules! assert_fmt {
        ($rendered:literal, $lit:expr) => {
            let val = $lit;
            assert_eq!($rendered, val.to_string());
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
    fn prim() {
        if let Some(unit) = TypeBuilder::new().add_prim(PrimitiveType::Unit).build("()") {
            assert_fmt!("()", unit);
        } else {
            panic!("expected unit struct!");
        }
    }
        
    
    //#[test]
    // fn option() {
    //     let unit = Type::new_prim(PrimitiveType::Unit);
    //     let unit_opt = Type::new_option(&unit);
    //     let t_u8 = Type::new_prim(PrimitiveType::U8);
    //     assert_fmt!("Option<()>", unit_opt.clone());
    //     assert_fmt!("Option<u8>", Type::new_option(&t_u8));
    //     assert_fmt!("Option<Option<()>>", Type::new_option(&unit_opt));
    // }
    // #[test]
    // fn seq() {
    //     assert_fmt!("[ () ; ? ]", Seq{length: None, typ: Box::new(TypeVariant::Unit)});
    //     assert_fmt!("[ () ; 5 ]", Seq{length: Some(5), typ: Box::new(TypeVariant::Unit)});
    // }
    // #[test]
    // fn map() {
    //     assert_fmt!("{ u8: () ; ? }", Map{length: None,
    //                                          key: Box::new(TypeVariant::U8),
    //                                         value: Box::new(TypeVariant::Unit)});
    //     assert_fmt!("{ u8: () ; 5 }", Map{length: Some(5),
    //                                          key: Box::new(TypeVariant::U8),
    //                                          value: Box::new(TypeVariant::Unit)});
    // }


}


// helper trait for various things to format themselves
trait TypeDisplay<I, T:Index<I>> {
    fn tfmt(&self, f: &mut fmt::Formatter<'_>, t: &T) -> fmt::Result;


    // this hack taken from https://github.com/rust-lang/rust/issues/46591#issue-280605901
    fn to_string(&self, t: &T) -> String
	where Self: Sized
    {
    let fmt = {
        struct ManualDisplay<'a,I, T:Index<I>>(&'a dyn TypeDisplay<I,T>, &'a T);

        impl<'a,I, T:Index<I>> fmt::Display for ManualDisplay<'a,I,T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.0.tfmt(f, self.1)
            }
        }

        ManualDisplay(self, t)
    };

	format!("{}", fmt)
    }
 
}

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
