use refptr::{make_refptr, refcounted, RefPtr};

#[refcounted(atomic)]
struct MyAtomic {
    aaa: i32,
    bbb: u32,
}

#[refcounted(local)]
struct MyNonatomic {
    aaa: i32,
    bbb: u32,
}

#[refcounted(atomic, weak)]
struct MyAtomicWeak {
    aaa: i32,
    bbb: u32,
}

#[refcounted(local, weak)]
struct MyNonatomicWeak {
    aaa: i32,
    bbb: u32,
}

#[test]
fn foo() {
    let _a: RefPtr<MyAtomic> = make_refptr!(MyAtomic { aaa: 5, bbb: 10 });
    let _c: RefPtr<MyNonatomic> = make_refptr!(MyNonatomic { aaa: 5, bbb: 10 });
    let _d: RefPtr<MyAtomicWeak> = make_refptr!(MyAtomicWeak { aaa: 5, bbb: 10 });
    let _d: RefPtr<MyNonatomicWeak> = make_refptr!(MyNonatomicWeak { aaa: 5, bbb: 10 });
}
