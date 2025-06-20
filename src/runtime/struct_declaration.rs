use std::collections::{HashMap, HashSet};

use crate::runtime::callable::CallableObj;

#[derive(Clone)]
pub struct StructDeclaration<'src> {
    pub name: &'src str,
    pub fields: HashSet<&'src str>,
    pub methods: HashMap<&'src str, CallableObj<'src>>,
}
