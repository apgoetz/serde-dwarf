use serde::de::{Deserialize,DeserializeSeed, Deserializer, Visitor};
use std::fmt;

use crate::typ::{Type, TypeKey, PrimitiveType};
use crate::Value;


impl Type {
    // helper function to deserialize based on a typekey. since typekey
    // is a typealias not a newtype, we can just implement DeserializeSeed on that type
    fn deserialize_key<'de, D:Deserializer<'de>>(&self, k: TypeKey, deserializer: D) -> Result<<Self as DeserializeSeed>::Value, D::Error> {
	use crate::typ::TypeVariant as TV;
	match self.parts[k] {
	    TV::Primitive(p) => {
		p.deserialize(deserializer)
	    }
	    _ => todo!()
	}
    }
}

impl<'de> DeserializeSeed<'de> for Type
{
    // The return type of the `deserialize` method. This implementation
    // appends onto an existing vector but does not create any new data
    // structure, so the return type is ().
    type Value = Value;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        self.deserialize_key(self.root, deserializer)
    }
}


macro_rules! deserialize_prim {
    ($self:expr, $deserializer:expr, $($prim:ident  =>  $rust:ty),*) => {
	match $self {
	    $(
		PrimitiveType::$prim => {
		    <$rust>::deserialize($deserializer).map(|v|Value::$prim(v))
		}
	    )*
            // we have to handle unit separately since it is the only
            // prim that is not a tuple variant
		PrimitiveType::Unit => <()>::deserialize($deserializer).map(|_| Value::Unit)
	}
    }
}

impl<'de> DeserializeSeed<'de> for PrimitiveType
{
    // The return type of the `deserialize` method. This implementation
    // appends onto an existing vector but does not create any new data
    // structure, so the return type is ().
    type Value = Value;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
	deserialize_prim!(self, deserializer,
			  Bool => bool,
			  U8 => u8,
			  U16 => u16,
			  U32 => u32,
			  U64 => u64,
			  U128 => u128,
			  I8 => i8,
			  I16 => i16,
			  I32 => i32,
			  I64 => i64,
			  I128 => i128,
			  F32 => f32,
			  F64 => f64,
			  Char => char,
			  String => String,
			  ByteArray => Box<[u8]>
			  // macro also handles () => Unit
                          
	)
    }
}

impl<'de> Visitor<'de> for PrimitiveType {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
	write!(formatter, "a {}", self)
    }
}
