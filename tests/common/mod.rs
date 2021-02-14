use serde::Deserializer;
use serde::de::{self,Visitor, IntoDeserializer, DeserializeSeed};
use std::cell::Cell;


// represents the various things that could be deserialized
#[derive(Debug, PartialEq)]
pub enum Tokens {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Char,
    Str,
    String,
    Bytes,
    ByteBuf,
    Option,
    Unit,
    UnitStruct(&'static str),
    NewTypeStruct(&'static str),
    Seq,
    Tuple(usize),
    TupleStruct(&'static str, usize),
    Map,
    Struct(&'static str, &'static [&'static str]),
    Enum(&'static str, &'static [&'static str]),
    UnitVariant(&'static str),
    NewTypeVariant(&'static str),
    TupleVariant(&'static str, usize),
    StructVariant(&'static str, &'static [&'static str]),
    I128,
    U128,
}

// a mock deserializer that remembers which methods were called on it
pub struct MockDeserializer<'a> {
    pub calls: &'a mut Vec<Tokens>,
    pub magic: u32,
}


impl<'a> MockDeserializer<'a> {
    pub fn new(calls: &'a mut Vec<Tokens>, magic: u32) -> Self {
	MockDeserializer{calls, magic}
    }
}

impl<'de, 'a> Deserializer<'de> for MockDeserializer<'a> {
    /// The error type that can be returned if some error occurs during
    /// deserialization.
    type Error = de::value::Error;


    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	unimplemented!();
    }

    /// Hint that the `Deserialize` type is expecting a `bool` value.
    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::Bool);
	visitor.visit_bool(self.magic > 0)
    }

    /// Hint that the `Deserialize` type is expecting an `i8` value.
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::I8);
	visitor.visit_i8(self.magic as i8)
    }

    /// Hint that the `Deserialize` type is expecting an `i16` value.
    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::I16);
	visitor.visit_i16(self.magic as i16)
    }

    /// Hint that the `Deserialize` type is expecting an `i32` value.
    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::I32);
	visitor.visit_i32(self.magic as i32)
    }

    /// Hint that the `Deserialize` type is expecting an `i64` value.
    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::I64);
	visitor.visit_i64(self.magic as i64)
    }

    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
	V: Visitor<'de>
    {	
	self.calls.push(Tokens::I128);
	visitor.visit_i128(self.magic as i128)
    }


    /// Hint that the `Deserialize` type is expecting a `u8` value.
    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::U8);
	visitor.visit_u8(self.magic as u8)
    }

    /// Hint that the `Deserialize` type is expecting a `u16` value.
    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::U16);
	visitor.visit_u16(self.magic as u16)
    }

    /// Hint that the `Deserialize` type is expecting a `u32` value.
    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::U32);
	visitor.visit_u32(self.magic as u32)
    }

    /// Hint that the `Deserialize` type is expecting a `u64` value.
    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::U64);
	visitor.visit_u64(self.magic as u64)
    }

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>
    {
	self.calls.push(Tokens::U128);
	visitor.visit_u128(self.magic as u128)
    }

    /// Hint that the `Deserialize` type is expecting a `f32` value.
    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::F32);
	visitor.visit_f32(self.magic as f32)
    }

    /// Hint that the `Deserialize` type is expecting a `f64` value.
    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::F64);
	visitor.visit_f64(self.magic as f64)
    }

    /// Hint that the `Deserialize` type is expecting a `char` value.
    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
        use std::convert::TryFrom;
	self.calls.push(Tokens::Char);
	visitor.visit_char(char::try_from(self.magic).unwrap())
    }

    /// Hint that the `Deserialize` type is expecting a string value and does
    /// not benefit from taking ownership of buffered data owned by the
    /// `Deserializer`.
    ///
    /// If the `Visitor` would benefit from taking ownership of `String` data,
    /// indiciate this to the `Deserializer` by using `deserialize_string`
    /// instead.
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::Str);
	visitor.visit_borrowed_str("foo")
    }

    /// Hint that the `Deserialize` type is expecting a string value and would
    /// benefit from taking ownership of buffered data owned by the
    /// `Deserializer`.
    ///
    /// If the `Visitor` would not benefit from taking ownership of `String`
    /// data, indicate that to the `Deserializer` by using `deserialize_str`
    /// instead.
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::String);
	visitor.visit_string(self.magic.to_string())
    }

    /// Hint that the `Deserialize` type is expecting a byte array and does not
    /// benefit from taking ownership of buffered data owned by the
    /// `Deserializer`.
    ///
    /// If the `Visitor` would benefit from taking ownership of `Vec<u8>` data,
    /// indicate this to the `Deserializer` by using `deserialize_byte_buf`
    /// instead.
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::Bytes);
	visitor.visit_borrowed_bytes(&[])
    }

    /// Hint that the `Deserialize` type is expecting a byte array and would
    /// benefit from taking ownership of buffered data owned by the
    /// `Deserializer`.
    ///
    /// If the `Visitor` would not benefit from taking ownership of `Vec<u8>`
    /// data, indicate that to the `Deserializer` by using `deserialize_bytes`
    /// instead.
    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::ByteBuf);
	visitor.visit_byte_buf(Vec::new())
    }

    /// Hint that the `Deserialize` type is expecting an optional value.
    ///
    /// This allows deserializers that encode an optional value as a nullable
    /// value to convert the null value into `None` and a regular value into
    /// `Some(value)`.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	// for now, we never visit options ( and therefore ignore part of the type )
	self.calls.push(Tokens::Option);
        if self.magic == 0 {
	    visitor.visit_none()
        } else {
            visitor.visit_some(MockDeserializer{
                calls: self.calls,
                magic: self.magic.saturating_sub(1)
            })
        }
    }

    /// Hint that the `Deserialize` type is expecting a unit value.
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::Unit);
	visitor.visit_unit()
    }

    /// Hint that the `Deserialize` type is expecting a unit struct with a
    /// particular name.
    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
        self.calls.push(Tokens::UnitStruct(name));
	visitor.visit_unit()
    }

    /// Hint that the `Deserialize` type is expecting a newtype struct with a
    /// particular name.
    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
        self.calls.push(Tokens::NewTypeStruct(name));
        let mut struct_tokens = Vec::new();
	let retval = visitor.visit_newtype_struct(MockDeserializer{
            calls: &mut struct_tokens,
            magic: self.magic,
        });
        self.calls.extend(struct_tokens.into_iter());
        retval
    }

    /// Hint that the `Deserialize` type is expecting a sequence of values.
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
        self.calls.push(Tokens::Seq);
        visitor.visit_seq(Access {
            calls: self.calls,
            len: self.magic as usize,
            magic: self.magic.saturating_sub(1),
        })
    }

    /// Hint that the `Deserialize` type is expecting a sequence of values and
    /// knows how many values there are without looking at the serialized data.
    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
        self.calls.push(Tokens::Tuple(len));
        visitor.visit_seq(Access {
            calls: self.calls,
            len,
            magic: self.magic
        })
    }

    /// Hint that the `Deserialize` type is expecting a tuple struct with a
    /// particular name and number of fields.
    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
        self.calls.push(Tokens::TupleStruct(name,len));
        visitor.visit_seq(Access {
            calls: self.calls,
            len,
            magic: self.magic,
        })
    }

    /// Hint that the `Deserialize` type is expecting a map of key-value pairs.
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
        self.calls.push(Tokens::Map);
        visitor.visit_map(Access {
            calls: self.calls,
            len: self.magic as usize,
            magic: self.magic.saturating_sub(1),
        })
    }

    /// Hint that the `Deserialize` type is expecting a struct with a particular
    /// name and fields.
    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::Struct(name, fields));
        visitor.visit_seq(Access {
            calls: self.calls,
            len: fields.len(),
            magic: self.magic,
        })
    }

    /// Hint that the `Deserialize` type is expecting an enum value with a
    /// particular name and possible variants.
    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	self.calls.push(Tokens::Enum(name, variants));
        visitor.visit_enum(EnumAccess{
            calls: self.calls,
            magic: self.magic,
            variants,
            sel_variant: Cell::new(None)})
    }

    /// Hint that the `Deserialize` type is expecting the name of a struct
    /// field or the discriminant of an enum variant.
    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	unimplemented!();
    }

    /// Hint that the `Deserialize` type needs to deserialize a value whose type
    /// doesn't matter because it is ignored.
    ///
    /// Deserializers for non-self-describing formats may not support this mode.
    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de> {
	unimplemented!();
    }

    fn is_human_readable(&self) -> bool {
        false
    }
}

struct Access<'a> {
    calls: &'a mut Vec<Tokens>,
    len: usize,
    magic: u32,
}

impl<'de, 'a> serde::de::SeqAccess<'de>
    for Access<'a>
{
    type Error = de::value::Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        if self.len > 0 {
            self.len -= 1;

            let value =
                serde::de::DeserializeSeed::deserialize(seed, MockDeserializer{
                    calls: self.calls,
                    magic: self.magic.saturating_sub(1)
                })?;
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.len)
    }
}

impl<'de, 'a> serde::de::MapAccess<'de>
    for Access<'a>
{
    type Error = de::value::Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        if self.len > 0 {
            self.len -= 1;
            let key =
                serde::de::DeserializeSeed::deserialize(seed, MockDeserializer{
                    calls: self.calls,
                    magic: self.magic
                })?;
            Ok(Some(key))
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let value = serde::de::DeserializeSeed::deserialize(seed, MockDeserializer{
            calls: self.calls,
            magic: self.magic,
        })?;
        Ok(value)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.len)
    }
}

struct EnumAccess<'a> {
    calls: &'a mut Vec<Tokens>,
    magic: u32,
    variants: &'static [&'static str],
    sel_variant: Cell<Option<&'static str>>,
}

impl<'de, 'a> serde::de::EnumAccess<'de>
    for EnumAccess<'a>
{
    type Error = de::value::Error;
    type Variant = Self;

     fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
        where
        V: serde::de::DeserializeSeed<'de>,
    {
         // we always return the first variant
         let idx: u32 = self.magic;
         self.sel_variant.set(Some(self.variants[idx as usize]));
         let val: Result<_,_> = seed.deserialize(idx.into_deserializer());
         Ok((val?, self))
     }

}

impl<'de, 'a> serde::de::VariantAccess<'de>
    for EnumAccess<'a>
{
    type Error = de::value::Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        self.calls.push(Tokens::UnitVariant(self.sel_variant.get().unwrap()));
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
        where
        T: DeserializeSeed<'de> {
        self.calls.push(Tokens::NewTypeVariant(self.sel_variant.get().unwrap()));
        seed.deserialize(MockDeserializer{
            calls: self.calls,
            magic: self.magic.saturating_sub(1)
        })
    }

    fn tuple_variant<V>(
    self, 
    len: usize, 
    visitor: V
    ) -> Result<V::Value, Self::Error>
        where
        V: Visitor<'de> {
        self.calls.push(Tokens::TupleVariant(self.sel_variant.get().unwrap(), len));
        visitor.visit_seq(Access {
            calls: self.calls,
            len,
            magic: self.magic.saturating_sub(1)
        })
    }

    fn struct_variant<V>(
        self, 
        fields: &'static [&'static str], 
        visitor: V
    ) -> Result<V::Value, Self::Error>
        where
        V: Visitor<'de> {
        self.calls.push(Tokens::StructVariant(self.sel_variant.get().unwrap(), fields));
        visitor.visit_seq(Access {
            calls: self.calls,
            len: fields.len(),
            magic: self.magic.saturating_sub(1),
        })
    }
}
