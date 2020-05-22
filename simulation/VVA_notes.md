# Validation, verification and accreditation

Real System --->  Conceptual Model ---> Computer program ---> Experiments & answers

Each transformation between each of these representations of the system presents a poteltial for erorr.
We verify each transformation.

Solution Validation: if the solutions provided with the simulation models, fits with the results we observe on the system, once we implemented on it the proposed solutions provided by the simulation model.

Each step on the model development has an associated VV&A
VV&A begins at the beginning of a program and that it is a cooperative and open activity.

Model creation: start from a simple unrealistic model and keep iterating until the model is good enough.

Errors of VV&A:
- Rejecting a valid model (risk of the modeller)
- Accepting an invalid model (risk for the customer) (dangerous)

Other errors:
- Type I erorr: "rejecting the null hypothesis when it is true"
- Type II erorr: "accepting the null hypothesis when it is false"
- Type III erorr: "solving the wrong problem [representation]"
- Type IV erorr: "the incorrect interpretation of a correctly rejected hypothesis"

Objectives of the VV&A:
- Produce a model that represents the system behaviour as close as possible to make it useful.

Accreditation: implies that the stakeholders believe on the model and use it to make previsions, that can be applied to the system.

## Techniques of VV&A

- Informal techniques: experts understand the inherent logic of the system, hence we ask about its behaviour to them.
- Formal techniques: calculation of the predicates (over-complicated, hard to implement)
- Static techniques: evaluate the static model design and the code used for its implementation. Formal construction of the simulation model (SDL, DEVs or Petri nets).
- Dynamic techniques (popular): analyze the results provided by the simulator. Using common statistical techniques.

## Difficulties of the VV&A

- No exist something called general validation
- It is possible that a "real world" does not exist to compare with the model
- What is the "real world"? Different roles have different visions of the system.
- Often the system data are not adequate (data may not exist, data might not represent all possibilities).
- The time: no time to validate and verify everything.
- Only can demonstrate that the model is wrong.

## Validation

- Data validation
- Conceptual model validation
- operational validation
- experimental validation
- solution validation

## Validity of the data

Type of data:
- Data for model construction.
- To test
- To experience the model validated.

Methods:
??

## Validity of the conceptual model

bla bla bla

## Verification

Verification is the process of comparing the program with the model and its behaviour with the real system.
