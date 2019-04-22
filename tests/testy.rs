use refcounted::{RefPtr, refcounted};

#[refcounted]
struct MyStruct {
    aaa: i32,
    bbb: u32,
}

#[refcounted(atomic)]
struct MyAtomic {
    aaa: i32,
    bbb: u32,
}

#[refcounted(nonatomic)]
struct MyNonatomic {
    aaa: i32,
    bbb: u32,
}


#[refcounted(atomic, weak)]
struct MyAtomicWeak {
    aaa: i32,
    bbb: u32,
}

#[refcounted(nonatomic, weak)]
struct MyNonatomicWeak {
    aaa: i32,
    bbb: u32,
}

#[test]
fn foo() {
    let _a: RefPtr<MyAtomic> = MyAtomic::alloc(5, 10);
    let _b: RefPtr<MyStruct> = MyStruct::alloc(5, 10);
    let _c: RefPtr<MyNonatomic> = MyNonatomic::alloc(5, 10);
    let _d: RefPtr<MyAtomicWeak> = MyAtomicWeak::alloc(5, 10);
    let _d: RefPtr<MyNonatomicWeak> = MyNonatomicWeak::alloc(5, 10);
}

