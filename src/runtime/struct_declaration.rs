use std::collections::{HashMap, HashSet};

use crate::runtime::callable::CallableObj;

#[derive(Clone)]
pub struct StructDeclaration {
    pub name: String,
    pub fields: HashSet<String>,
    pub methods: HashMap<String, CallableObj>,
}
