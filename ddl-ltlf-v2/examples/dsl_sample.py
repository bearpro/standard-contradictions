import mdl


def main() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.now = mdl.Proposition("", "now")
        doc.is_x = mdl.Proposition("x")
        doc.is_not_x = mdl.Not(mdl.Proposition("x"))
        doc.now_x = mdl.Until(doc.now, doc.is_x)
        doc.now_not_x = mdl.Until(doc.now, doc.is_not_x)
        
        doc.r1 = mdl.Rule("test1", "O", None, doc.now_x)
        doc.r2 = mdl.Rule("test2", "O", None, doc.now_not_x)

    result = mdl.solve(doc.build(), horizon=1)
    assert not result.is_consistent


if __name__ == "__main__":
    main()
