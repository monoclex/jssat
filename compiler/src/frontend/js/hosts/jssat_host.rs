use super::HostEnvironment;

/// JSSAT host environment - this hooks functions useful
pub struct JSSATHostEnvironment {}

impl JSSATHostEnvironment {
    pub fn new() -> Self {
        Self {}
    }
}

impl HostEnvironment for JSSATHostEnvironment {
    fn inject<'s>(&mut self, hook: &super::HostHookState<'s>) {
        // todo!()
    }
}
