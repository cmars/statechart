#[cfg(test)]

use std::cell::RefCell;
use std::rc::Rc;

extern crate statechart;
use statechart::*;

fn a_eq_x(x: i32) -> Rc<Fn(&Context) -> bool> {
    Rc::new(move |ctx: &Context| -> bool {
        match ctx.get_var("a") {
            Some(&Value::Int(n)) => n == x,
            _ => false,
        }
    })
}

#[test]
fn first_choice_only_match() {
    let sc = states!("S", substates: substates!(
        state!("S0",
            transitions: vec![goto!(target_label: Some("SF".to_string()))],
            on_entry: vec![action_assign!(key: "a", value: Value::Int(1))],
            on_exit: vec![action_choose!(when: vec![
                (cond_fn!(a_eq_x(1)),
                    Box::new(action_assign!(key: "b",
                        value: Value::String("matched".to_string()))))])]),
        final_state!("SF",
            result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()))));
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}

#[test]
fn last_match() {
    let sc = states!("S", substates: substates!(
        state!("S0",
            transitions: vec![goto!(target_label: Some("SF".to_string()))],
            on_entry: vec![action_assign!(key: "a", value: Value::Int(2))],
            on_exit: vec![action_choose!(when: vec![
                (cond_fn!(a_eq_x(1)), Box::new(action_assign!(
                    key: "b", value: Value::String("not matched".to_string())))),
                (cond_fn!(a_eq_x(2)), Box::new(action_assign!(
                    key: "b", value: Value::String("matched".to_string())))),
            ])]),
        final_state!("SF",
            result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()))));
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}

#[test]
fn otherwise() {
    let sc = states!("S", substates: substates!(
        state!("S0",
            transitions: vec![goto!(target_label: Some("SF".to_string()))],
            on_exit: vec![action_choose!(
                when: vec![
                    (cond_fn!(a_eq_x(1)), Box::new(action_assign!(
                        key: "b", value: Value::String("not matched".to_string())))),
                    (cond_fn!(a_eq_x(2)), Box::new(action_assign!(
                        key: "b", value: Value::String("not matched".to_string())))),
                ],
                otherwise: Some(Box::new(action_assign!(
                    key: "b", value: Value::String("matched".to_string())))))]),
        final_state!("SF",
            result: Output::ValueOf(ValueOfBuilder::default().key("b").build().unwrap()))));
    let mut ctx = Context::new(sc);
    let result = ctx.run();
    assert!(result.is_ok(), "fault: {:?}", result.err().unwrap());
    assert_eq!(result.unwrap(), Value::String("matched".to_string()));
}
