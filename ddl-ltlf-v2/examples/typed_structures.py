import mdl


def main() -> None:
    person_type = mdl.ProductType("Person", {"age": mdl.INT, "name": mdl.STRING})
    decision_type = mdl.SumType(
        "Decision",
        {"Approved": person_type, "Rejected": mdl.STRING},
    )

    with mdl.ModuleBuilder() as doc:
        person = mdl.Var("person", person_type)
        decision = mdl.Var("decision", decision_type)
        approved = mdl.VariantPayload(decision, "Approved")
        rejected = mdl.VariantPayload(decision, "Rejected")

        doc.person_type = person_type
        doc.decision_type = decision_type
        doc.person = person
        doc.decision = decision
        doc.adult_person = mdl.Rule("adult-person", "O", None, person.field("age") >= 18)
        doc.named_person = mdl.Rule(
            "named-person",
            "O",
            None,
            mdl.Len(person.field("name")) > 0,
        )
        doc.valid_decision = mdl.Rule(
            "valid-decision",
            "O",
            None,
            mdl.Case(
                decision,
                {
                    "Approved": approved.field("age") >= 18,
                    "Rejected": mdl.Len(rejected) > 0,
                },
            ),
        )

    result = mdl.solve(doc.build())
    assert result.is_consistent


if __name__ == "__main__":
    main()
