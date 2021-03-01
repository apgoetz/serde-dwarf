use serde::de::{Deserialize,DeserializeSeed, Deserializer, Visitor, SeqAccess, EnumAccess, VariantAccess};
use serde::de::Error as SerdeError;
use std::fmt;
use indexmap::IndexMap;

use crate::typ::{Type, SubType, TypeKey, PrimitiveType};
use crate::Value;
// TODO: use a better name :(
use crate::Variant as ValVariant;
use crate::typ::TypeVariant as TV;
use crate::typ::Variant;


const MAX_FIELDS : usize = 64;
static FIELD_SLICE: [&str; MAX_FIELDS] = ["", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                          "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                          "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
                                          "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""];
                                          

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
	SubType::new(&self).deserialize(deserializer)
    }
}


impl<'a, 'de> DeserializeSeed<'de> for SubType<'a>
{
    // The return type of the `deserialize` method. This implementation
    // appends onto an existing vector but does not create any new data
    // structure, so the return type is ().
    type Value = Value;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        
	match &self.parts[self.root] {
	    TV::Primitive(p) => {
		p.deserialize(deserializer)
	    }
            TV::Tuple(types) => {
                deserializer.deserialize_tuple(types.len(), TupleVisitor(&self, types, TupleMarker))
            }
            TV::Option(k) => {
                deserializer.deserialize_option(OptionVisitor(self.new_key(*k)))
            }
            TV::UnitStruct(name) => {
                // TODO: review what to do for 'static lifetime names. most serde serializers,
                // deserializers dont seem to even use the name field
                deserializer.deserialize_unit_struct("", UnitStructVisitor(name.to_string()))
            }
            TV::NewType(name, k) => {
                // TODO: review what to do for 'static lifetime names. most serde serializers,
                // deserializers dont seem to even use the name field
                deserializer.deserialize_newtype_struct("", NewTypeVisitor(name.to_string(), self.new_key(*k)))
            }
            TV::TupleStruct(name, types) => {
                // TODO: review what to do for 'static lifetime names. most serde serializers / 
                // deserializers dont seem to even use the name field
                deserializer.deserialize_tuple_struct("", types.len(), TupleVisitor(&self, types, TupleStructMarker(name.to_string())))
            }
            TV::Struct(name, types) => {
                let i = types.len();
                if i > MAX_FIELDS {
                    return Err(D::Error::custom(format!("Cannot deserialize type with more than {} fields!", MAX_FIELDS)));
                }

                // TODO: review what to do for 'static lifetime names. most serde serializers /
                // deserializers dont seem to even use the name field however, we do need to pass
                // in the right number of fields, in the field names parameter, so we do that with
                // a fake static slice
                // TODO: implement for map style deserializers, instead of seq style deserializers
                let marker = StructMarker(name.to_string(), &types);
                deserializer.deserialize_struct("", &FIELD_SLICE[0..i], TupleVisitor(&self, &types.values().copied().collect::<Vec<_>>(), marker))
            }
            TV::Enum(_, variants) => {
                let i = variants.len();
                if i > MAX_FIELDS {
                    return Err(D::Error::custom(format!("Cannot deserialize enum with more than {} variants!", MAX_FIELDS)));
                }
                deserializer.deserialize_enum("", &FIELD_SLICE[0..i], EnumVisitor(&self, variants))
            }
	    _ => todo!()
	}
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

trait Marker {
    fn build_result(self, fields: Vec<Value>) -> Value;
}

struct TupleMarker;
impl Marker for TupleMarker {
    fn build_result(self, fields: Vec<Value>) -> Value {
        Value::Tuple(fields)
    }
}
struct TupleStructMarker(String);
impl Marker for TupleStructMarker {
    fn build_result(self, fields: Vec<Value>) -> Value {
        Value::TupleStruct(self.0, fields)
    }
}

struct StructMarker<'a>(String, &'a IndexMap<String, TypeKey>);
impl<'a> Marker for StructMarker<'a> {
    fn build_result(self, fields: Vec<Value>) -> Value {
        Value::Struct(self.0, self.1.keys().map(String::from).zip(fields).collect::<Vec<_>>())
    }
}

struct TupleVariantMarker(String, String);
impl Marker for TupleVariantMarker {
    fn build_result(self, fields: Vec<Value>) -> Value {
        Value::Enum(self.0,ValVariant::TupleVariant(self.1, fields))
    }
}

struct StructVariantMarker<'a>(String, String, &'a IndexMap<String, TypeKey>);
impl<'a> Marker for StructVariantMarker<'a> {
    fn build_result(self, fields: Vec<Value>) -> Value {
        Value::Enum(self.0, ValVariant::StructVariant(self.1, self.2.keys().map(String::from).zip(fields).collect::<Vec<_>>()))
    }
}



// tuple visistor used for tuples and tuplestructs
struct TupleVisitor<'a, M:Marker>(&'a SubType<'a>, &'a[TypeKey], M);

impl<'a, 'de, M:Marker> Visitor<'de> for TupleVisitor<'a, M> {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
	formatter.write_str("a tuple")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> 
	where A: SeqAccess<'de>
    {
	let mut fields : Vec<Value> =
	    if let Some(length) = seq.size_hint() {
		Vec::with_capacity(length)
	    } else {
		Vec::new()
	    };

	// for each type key in the tuple
	for k in self.1 {
	    // if this element is in the tuple
	    if let Some(value) = seq.next_element_seed(self.0.new_key(*k))? {
		fields.push(value);
	    } else {
		// the seq returned None while we still had fields left to parse
		return Err(A::Error::custom("tuple had too few fields"));
	    }
	}
        Ok(self.2.build_result(fields))
    }
}

struct OptionVisitor<'a>(SubType<'a>);

impl<'a, 'de> Visitor<'de> for OptionVisitor<'a> {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "Option<{}>", self.0.name())
    }

    // unit and none are both treated as none as per serde
    // TODO: add test to trigger this visit fn
    #[inline]
    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Value::Option(None))
    }

    #[inline]
    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Value::Option(None))
    }

    #[inline]
    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        self.0.deserialize(deserializer).map(|v| Value::Option(Some(Box::from(v))))
    }
    
}

struct UnitStructVisitor(String);

impl<'de> Visitor<'de> for UnitStructVisitor {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "unit struct {}", self.0)
    }

    #[inline]
    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Value::UnitStruct(self.0))
    }
}

struct NewTypeVisitor<'a>(String, SubType<'a>);

impl<'a, 'de> Visitor<'de> for NewTypeVisitor<'a> {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "newtype {}", self.0)
    }


    #[inline]
    fn visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        let Self(name, typ) = self;
        typ.deserialize(deserializer).map(|v| Value::NewType(name, Box::from(v)))
    }
    
}

// tuple visistor used for tuples and tuplestructs
struct EnumVisitor<'a>(&'a SubType<'a>, &'a[Variant<TypeKey>]);

impl<'a, 'de> Visitor<'de> for EnumVisitor<'a> {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("an enum")
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
        where
    A: EnumAccess<'de>, 
    {
        // deserialize, but only via index
        let (variant, vaccess) = data.variant::<u32>()?;
        let variant = variant as usize;
        if variant >= self.1.len() {
            return Err(A::Error::custom("discriminator too big for enum"));
        }
        
        // process the variant of the enum
        match &self.1[variant] {
            Variant::Unit(name) => {
                vaccess.unit_variant()?;
                Ok(Value::Enum(self.0.name(), ValVariant::UnitVariant(String::from(name))))
            }
            Variant::NewType(name, k) => {
                let value = vaccess.newtype_variant_seed(self.0.new_key(*k))?;
                Ok(Value::Enum(self.0.name(), ValVariant::NewTypeVariant(String::from(name), Box::from(value))))
            }
            Variant::Tuple(name, fields) => {
                vaccess.tuple_variant(fields.len(), TupleVisitor(self.0, fields, TupleVariantMarker(self.0.name(), name.to_string())))
            }
            Variant::Struct(name, fields) => {
                let types = &fields.values().copied().collect::<Vec<_>>();
                vaccess.tuple_variant(fields.len(), TupleVisitor(self.0, types, StructVariantMarker(self.0.name(), name.to_string(), fields)))
            }
        }
    }
}
