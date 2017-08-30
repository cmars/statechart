#[cfg(test)]

extern crate env_logger;
extern crate statechart;
use statechart::*;

#[test]
fn assign_string() {
    let _ = env_logger::init();
    let sc = states!{ S {
        substates: [
            state!{ S1 {
                transitions: [goto!(target: S2)],
                on_entry: [action_assign!(key: "hello", value: Value::from_str("assign"))],
            }},
            final_state!{ S2 {
                result: Output::ValueOf(ValueOfBuilder::default().key("hello").build().unwrap()),
            }},
        ]}};
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::from_str("assign"));
}
