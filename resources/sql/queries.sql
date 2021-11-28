-- :name create-formula!
-- :doc creates a new formula from an edn string
INSERT INTO formula
(formula)
VALUES (:formula)

-- :name create-assumption!
-- :doc creates a new assumption for a goal
INSERT INTO assumption
(formula_id, goal_id)
VALUES (:formula_id :goal_id)

-- :name create-goal!
-- :doc creates a new goal
INSERT INTO goal
(goal_formula_id, proof_id)
VALUES (:formula-id :proof-id)

-- :name create-proof!
-- :doc creates a new proof
INSERT INTO proof ;

-- :name get-formula-id :? :1
-- :doc get formula from canonical edn representation
SELECT id
FROM formula
WHERE formula = :formula

-- :name get-goals-for-proof
-- :doc gets all the goals for the current proof
SELECT id
FROM goal
WHERE proof_id = :proof-id

-- :name get-assumptions
-- :doc get all the assumptions for a goal 
SELECT formula
 FROM formula form
  JOIN assumption assum
    ON assum.forula_id = form.id
  JOIN goal g
    ON assum.goal_id=g.id
WHERE g.id = :goal-id) AS assumptions

-- :name get-goal
-- :doc get a goal by it's id
SELECT formula
 FROM formula form
  JOIN goal g
  ON form.id=g.goal_formula_id
 WHERE g.id= :goal0d

-- :name get-premises
-- :doc get the premises from a proof
SELECT formula
FROM formula form
 JOIN premise p
   ON p.formula_id = form.id
 WHERE p.proof_id = :proof-id

-- :name get-goal-ids
-- :doc get the goal ids for a proof
SELECT id
 FROM goal
 WHERE proof_id = :proof-id

-- :name tables
-- :doc explore
SELECT * FROM INFORMATION_SCHEMA.TABLES

-- :name schema-migrations
SELECT * FROM SCHEMA_MIGRATIONS

-- :name clean-schema-migrations
DELETE FROM SCHEMA_MIGRATIONS


