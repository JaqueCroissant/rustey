use crate::eval::Object;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    store: Vec<(String, Object)>,
    outer: Option<Box<Environment>>
}

impl Environment{
    pub fn new() -> Environment {
        Environment { store: vec![], outer: None }
    }

    pub fn new_enclosed(outer: Environment) -> Environment{
        Environment { store: vec![], outer: Some(Box::new(outer)) }
    }

    pub fn set(&mut self, name: String, object: Object) -> Result<(), String> {
        if self.store.contains(&(name.clone(), object.clone())){
            return Err(format!("value already exists in store"));
        }

        self.store.push((name.clone(), object));
        return Ok(());
    }

    pub fn get(&mut self, name: String) -> Option<Object> {
        
        match self.store.iter().find(|&(x, _)| x == &name){
            Some(x) => return Some(x.1.clone()),
            None => {
                match self.outer {
                    None => return None,
                    Some(_) => return self.outer.clone().unwrap().get(name)
                }
            }
        }
    }
}