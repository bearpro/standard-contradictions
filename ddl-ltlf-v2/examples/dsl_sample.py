import mdl

def main() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.r1 = mdl.Rule("test1")
        doc.r2 = mdl.Rule("test2")
    
    doc = doc.build()

    mdl.solve(doc)


if __name__ == "__main__":
    main()
