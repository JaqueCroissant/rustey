use std::fmt::Display;

pub struct Object<T> {
    obj_type: ObjectType,
    value: T
}

impl<T> Object<T> {
    pub fn new(obj_type: ObjectType, value: T) -> Object<T> where T: Display{
        Object { obj_type, value }
    }

    pub fn inspect(&self) -> String{
        match &self.obj_type {
            ObjectType::Null => "null".to_string(),
            _ => self.value.to_string()
        }
    }

    
}

impl<T> std::fmt::Display for Object<T> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.name, self.age)
    }
}

pub enum ObjectType {
    Integer,
    Bool,
    Null
}