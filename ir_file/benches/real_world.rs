use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("sample-size");
    group.sample_size(10);

    group.bench_function("real world", |b| {
        b.iter(|| {
            ir_file::generate(
                "ECMA262Methods",
                black_box(include_str!(
                    "../../jssat_frontend_js/src/ecmascript/ECMA262Methods.lisp"
                )),
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
