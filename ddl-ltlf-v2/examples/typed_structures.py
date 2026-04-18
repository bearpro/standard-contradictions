import mdl


def main() -> None:
    with mdl.ModuleBuilder() as doc:
        doc.person_type = mdl.ProductType({"age": mdl.INT, "name": mdl.STRING})
        doc.decision_type = mdl.SumType(
            {"Approved": doc.person_type, "Rejected": mdl.STRING},
        )
        doc.person = mdl.Var(doc.person_type)
        doc.decision = mdl.Var(doc.decision_type)
        doc.approved = mdl.VariantPayload(doc.decision, "Approved")
        doc.rejected = mdl.VariantPayload(doc.decision, "Rejected")

        doc.adult_person = mdl.Rule("O", None, doc.person.field("age") >= 18)
        doc.named_person = mdl.Rule(
            "O",
            None,
            mdl.Len(doc.person.field("name")) > 0,
        )
        doc.valid_decision = mdl.Rule(
            "O",
            None,
            mdl.Case(
                doc.decision,
                {
                    "Approved": doc.approved.field("age") >= 18,
                    "Rejected": mdl.Len(doc.rejected) > 0,
                },
            ),
        )

    result = mdl.solve(doc.build())
    assert result.is_consistent


if __name__ == "__main__":
    main()
