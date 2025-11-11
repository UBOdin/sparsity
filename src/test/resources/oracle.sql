-- Oracle SQL test file
-- This file contains comprehensive SQL statements to exercise Oracle parser functionality

-- PROMPT statement (SQL*Plus directive)
PROMPT Starting Oracle SQL script
/

-- SET DEFINE OFF (SQL*Plus directive)
SET DEFINE OFF
/

-- COMMIT statement
COMMIT
/

-- CREATE TABLE with Oracle-specific data types and clauses
CREATE TABLE employees (
  emp_id NUMBER(10) PRIMARY KEY,
  first_name VARCHAR2(50 BYTE) NOT NULL,
  last_name VARCHAR2(50 CHAR) NOT NULL,
  email NVARCHAR2(100),
  salary NUMBER(10, 2),
  hire_date DATE,
  bio CLOB,
  photo BLOB,
  raw_data RAW(16),
  status VARCHAR(20) DEFAULT 'ACTIVE'
) TABLESPACE users PCTUSED 40 PCTFREE 10 INITRANS 2 MAXTRANS 255 STORAGE (
  INITIAL 64K
  NEXT 64K
  MINEXTENTS 1
  MAXEXTENTS UNLIMITED
  PCTINCREASE 0
  BUFFER_POOL DEFAULT
) LOGGING COMPRESS CACHE PARALLEL MONITORING
/

-- CREATE TABLE AS SELECT
CREATE TABLE emp_backup AS SELECT * FROM employees
/

-- CREATE OR REPLACE TABLE
CREATE OR REPLACE TABLE departments (
  dept_id NUMBER(5) PRIMARY KEY,
  dept_name VARCHAR2(100),
  location VARCHAR2(50)
) TABLESPACE users NOLOGGING NOCOMPRESS NOCACHE NOPARALLEL NOMONITORING
/

-- CREATE MATERIALIZED VIEW
CREATE MATERIALIZED VIEW emp_summary AS SELECT emp_id, COUNT(*) as cnt FROM employees GROUP BY emp_id
/

-- CREATE OR REPLACE MATERIALIZED VIEW
CREATE OR REPLACE MATERIALIZED VIEW dept_summary AS SELECT dept_id, AVG(salary) as avg_sal FROM employees GROUP BY dept_id
/

-- CREATE TEMPORARY VIEW
CREATE TEMPORARY VIEW temp_emp AS SELECT * FROM employees WHERE salary > 50000
/

-- CREATE OR REPLACE VIEW
CREATE OR REPLACE VIEW active_employees AS SELECT * FROM employees WHERE status = 'ACTIVE'
/

-- ALTER VIEW MATERIALIZE
ALTER VIEW active_employees MATERIALIZE
/

-- ALTER VIEW DROP MATERIALIZE
ALTER VIEW active_employees DROP MATERIALIZE
/

-- GRANT statement
GRANT SELECT ON employees TO hr_user
/

-- GRANT multiple privileges
GRANT SELECT, UPDATE, INSERT, DELETE ON departments TO admin_user
/

-- GRANT ALL
GRANT ALL ON employees TO super_admin
/

-- INSERT with VALUES
INSERT INTO employees (emp_id, first_name, last_name, salary) VALUES (1, 'John', 'Doe', 50000)
/

-- INSERT OR REPLACE with VALUES
INSERT OR REPLACE INTO employees (emp_id, first_name, last_name) VALUES (2, 'Jane', 'Smith')
/

-- INSERT with multiple VALUES
INSERT INTO employees (emp_id, first_name, last_name) VALUES (3, 'Bob', 'Jones'), (4, 'Alice', 'Brown')
/

-- INSERT with SELECT
INSERT INTO employees (emp_id, first_name, last_name) SELECT emp_id + 100, first_name, last_name FROM employees WHERE emp_id < 3
/

-- INSERT OR REPLACE with SELECT
INSERT OR REPLACE INTO employees SELECT * FROM emp_backup WHERE emp_id > 10
/

-- UPDATE statement
UPDATE employees SET salary = 60000 WHERE emp_id = 1
/

-- UPDATE with multiple SET clauses
UPDATE employees SET salary = salary * 1.1, status = 'UPDATED' WHERE emp_id = 2
/

-- UPDATE without WHERE
UPDATE departments SET location = 'UNKNOWN'
/

-- DELETE statement
DELETE FROM employees WHERE emp_id = 100
/

-- DELETE without WHERE
DELETE FROM emp_backup
/

-- SELECT statement
SELECT emp_id, first_name, last_name FROM employees
/

-- SELECT with WHERE
SELECT * FROM employees WHERE salary > 50000 AND status = 'ACTIVE'
/

-- SELECT with JOIN
SELECT e.emp_id, e.first_name, d.dept_name FROM employees e JOIN departments d ON e.dept_id = d.dept_id
/

-- SELECT with GROUP BY and HAVING
SELECT dept_id, AVG(salary) as avg_salary FROM employees GROUP BY dept_id HAVING AVG(salary) > 50000
/

-- SELECT with ORDER BY
SELECT emp_id, first_name, last_name, salary FROM employees ORDER BY salary DESC, last_name ASC
/

-- SELECT with UNION
SELECT emp_id, first_name FROM employees UNION SELECT emp_id, first_name FROM emp_backup
/

-- SELECT with subquery (CTEs not yet supported in parser)
SELECT * FROM (SELECT * FROM employees WHERE salary > 100000) high_salary
/

-- EXPLAIN statement
EXPLAIN SELECT * FROM employees WHERE emp_id = 1
/

-- DROP TABLE
DROP TABLE emp_backup
/

-- DROP TABLE IF EXISTS
DROP TABLE IF EXISTS temp_table
/

-- DROP VIEW
DROP VIEW temp_emp
/

-- DROP VIEW IF EXISTS
DROP VIEW IF EXISTS non_existent_view
/

-- CREATE TABLE with NUMBER precision and scale
CREATE TABLE products (
  product_id NUMBER(10, 0) PRIMARY KEY,
  price NUMBER(10, 2),
  discount NUMBER(5, 4),
  quantity NUMBER
)
/

-- CREATE TABLE with various column constraints
CREATE TABLE orders (
  order_id NUMBER PRIMARY KEY,
  customer_id NUMBER NOT NULL,
  order_date DATE DEFAULT SYSDATE,
  total_amount NUMBER(10, 2) NOT NULL,
  INDEX ON customer_id,
  INDEX ON (order_date, customer_id)
)
/

-- CREATE TABLE with storage clause variations
CREATE TABLE large_table (
  id NUMBER,
  data VARCHAR2(4000)
) STORAGE (
  INITIAL 1M
  NEXT 2M
  MINEXTENTS 5
  MAXEXTENTS 100
  PCTINCREASE 10
  BUFFER_POOL KEEP
)
/

-- Comments in SQL (should be handled by whitespace parser)
-- This is a comment
SELECT 1 FROM DUAL
/
-- Another comment
SELECT 2 FROM DUAL
/

-- Multi-line statement
CREATE TABLE complex_table (
  col1 NUMBER(10),
  col2 VARCHAR2(100),
  col3 DATE,
  CONSTRAINT pk_complex PRIMARY KEY (col1)
)
TABLESPACE users
PCTUSED 50
PCTFREE 20
LOGGING
/

-- Final COMMIT
COMMIT
/

-- CREATE TABLE with DOUBLE PRECISION
CREATE TABLE precision_table (
  id NUMBER PRIMARY KEY,
  value1 DOUBLE PRECISION,
  value2 DOUBLE PRECISION NOT NULL,
  value3 DOUBLE PRECISION DEFAULT 0.0
)
/

-- CREATE TABLE with SMALLINT
CREATE TABLE smallint_table (
  id NUMBER PRIMARY KEY,
  small_value SMALLINT,
  small_value_not_null SMALLINT NOT NULL,
  small_value_default SMALLINT DEFAULT 0
)
/

-- CREATE TABLE with DOUBLE PRECISION and SMALLINT together
CREATE TABLE mixed_types (
  id NUMBER PRIMARY KEY,
  group_no SMALLINT NOT NULL ENABLE,
  group_name VARCHAR2(50 BYTE),
  mm_spread_factor DOUBLE PRECISION,
  stress_spread_factor DOUBLE PRECISION,
  rating_limit DOUBLE PRECISION,
  min_rate DOUBLE PRECISION,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
)
/

-- Final PROMPT
PROMPT Oracle SQL script completed successfully
/

-- CREATE TABLE with NUMBER(*,0) - Oracle allows * for unspecified precision
CREATE TABLE test_number_star (
  id NUMBER(*,0) NOT NULL ENABLE,
  value NUMBER(*,0) NOT NULL ENABLE
)
/

-- CREATE TABLE with DEFAULT ON NULL
CREATE TABLE test_default_on_null (
  id NUMBER PRIMARY KEY,
  created_by VARCHAR2(32 BYTE) DEFAULT ON NULL 'SYSTEM',
  updated_by VARCHAR2(32 BYTE) DEFAULT ON NULL 'SYSTEM',
  created_at DATE DEFAULT ON NULL SYSDATE,
  updated_at DATE DEFAULT ON NULL SYSDATE
)
/

-- ALTER TABLE MODIFY with DEFAULT ON NULL
ALTER TABLE test_default_on_null MODIFY( created_by VARCHAR2(32 BYTE) DEFAULT ON NULL 'GUI_XMDM' )
/

ALTER TABLE test_default_on_null MODIFY( updated_by VARCHAR2(32 BYTE) DEFAULT ON NULL 'GUI_XMDM' )
/

ALTER TABLE test_default_on_null MODIFY( created_at DATE DEFAULT ON NULL SYSDATE )
/

ALTER TABLE test_default_on_null MODIFY( updated_at DATE DEFAULT ON NULL SYSDATE )
/

-- CREATE VIEW with database link (@ syntax)
CREATE OR REPLACE VIEW test_db_link_view AS
SELECT *
FROM schema1.table1@database_link
/

-- SELECT with database link
SELECT * FROM schema2.table2@remote_db
/

-- DROP SYNONYM
DROP SYNONYM schema1.synonym_name
/

-- DROP SYNONYM IF EXISTS
DROP SYNONYM IF EXISTS schema1.synonym_name
/

-- CREATE SYNONYM
CREATE SYNONYM schema1.synonym_name FOR table_name@database_link
/

-- CREATE OR REPLACE SYNONYM
CREATE OR REPLACE SYNONYM schema1.synonym_name FOR table_name@database_link
/

-- ALTER TABLE MODIFY without parentheses (single column)
ALTER TABLE test_table MODIFY column_name VARCHAR(70)
/

-- ALTER TABLE MODIFY without parentheses (multiple columns)
ALTER TABLE test_table MODIFY col1 VARCHAR(60), MODIFY col2 VARCHAR(30)
/

-- ALTER TABLE RENAME COLUMN
ALTER TABLE test_table RENAME COLUMN old_name TO new_name
/

-- ALTER TABLE ADD column
ALTER TABLE test_table ADD new_column VARCHAR(70)
/

-- ALTER TABLE ADD column with constraints
ALTER TABLE test_table ADD new_column VARCHAR(70) NOT NULL DEFAULT 'value'
/

-- ALTER TABLE ADD PRIMARY KEY constraint
ALTER TABLE test_table ADD PRIMARY KEY (col1, col2)
/

-- ALTER TABLE ADD CONSTRAINT PRIMARY KEY
ALTER TABLE test_table ADD CONSTRAINT pk_test PRIMARY KEY (col1)
/

-- ALTER TABLE ADD INDEX ON
ALTER TABLE test_table ADD INDEX ON (col1, col2)
/

-- ALTER TABLE ADD with multiple ADD clauses including constraints
ALTER TABLE test_table
    ADD col1 NUMBER
    ADD PRIMARY KEY (col1)
/

-- INSERT with long string value containing dots
INSERT INTO test_table (col1, col2) VALUES ('value1', 'de.deutsche_boerse.statistix.xmdm.helpers.CommonHelper.getUserId')
/

-- INSERT with multiple long string values
INSERT INTO test_table (col1, col2, col3) VALUES ('RICA', 'DV_STA_SP_DEF_CALIB_GRP', 'de.deutsche_boerse.statistix.xmdm.helpers.CommonHelper.getUserId')
/

-- DELETE without FROM keyword (Oracle allows this)
DELETE test_table WHERE col1 = 'value'
/

-- DELETE without FROM and without WHERE
DELETE test_table
/

-- UPDATE with table alias
UPDATE test_table alias SET alias.col1 = 'value' WHERE alias.col2 = 'test'
/

-- UPDATE with schema.table and alias
UPDATE schema.table alias SET col1 = col1 + 1 WHERE col2 = 'test'
/

-- ALTER TABLE DROP COLUMN
ALTER TABLE test_table DROP COLUMN col1
/

-- ALTER TABLE DROP COLUMN with schema
ALTER TABLE schema.table DROP COLUMN col1
/

-- Comments that look like SQL statements (should be skipped)
--DROP TABLE test_table
/

--delete from test_table WHERE col1 = 'value'
/

--select * from test_table
/

-- ALTER TABLE ADD with newlines
ALTER TABLE test_table
  ADD new_col NUMBER
/

-- ALTER TABLE ADD with multiple newlines
ALTER TABLE test_table

    ADD another_col VARCHAR(50)

/

-- DROP TABLE with schema.table format
DROP TABLE schema.table
/

-- DROP TABLE IF EXISTS with schema.table
DROP TABLE IF EXISTS schema.table
/

-- Comments with SQL statements (should be skipped by parser)
--DROP TABLE test_table
/

--delete from test_table WHERE col1 = 'value'
/

--select rowid, A.* from test_table A where col1 = 'value'
/

--INSERT INTO test_table (col1, col2) VALUES ('value1', 'de.deutsche_boerse.statistix.xmdm.helpers.CommonHelper.getUserId');
/

-- ALTER TABLE DROP COLUMN with schema.table format
ALTER TABLE schema.table DROP COLUMN col1
/

-- UPDATE with schema.table and alias
UPDATE schema.table alias SET alias.col1 = 'value' WHERE alias.col2 = 'test'
/

-- UPDATE with schema.table (no alias)
UPDATE schema.table SET col1 = col1 + 1 WHERE col2 = 'test'
/

-- DELETE without FROM with schema.table
DELETE schema.table WHERE col1 = 'value'
/

-- ALTER TABLE ADD with newlines
ALTER TABLE test_table
  ADD new_col NUMBER
/

-- ALTER TABLE ADD with multiple newlines
ALTER TABLE test_table

    ADD another_col VARCHAR(50)

/


-- Test cases for reported errors

-- Multi-line comments with SQL-like text (should be completely ignored)
--hidden fields
--INSERT INTO schema.table (col1, col2) VALUES
--('value1', 'package.subpackage.ClassName.methodName');
/

-- Another multi-line comment with SQL
--INSERT INTO schema.table (col1, col2) VALUES
--('value2', 'package.subpackage.ClassName.methodName');
/

-- UPDATE with schema.table format (was failing at column 33)
UPDATE schema.table SET col1 = col1 + 1 WHERE col2 = 'test'
/

-- UPDATE with schema.table where column name appears after table (edge case - was failing)
UPDATE schema.table col_name SET col_name = col_name + 1 WHERE col2 = 'test'
/

-- DELETE without FROM with schema.table format (was failing)
DELETE schema.table WHERE col1 = 'value'
/

-- ALTER TABLE DROP COLUMN with schema.table format (was failing)
ALTER TABLE schema.table DROP COLUMN col1
/

-- ALTER TABLE ADD with multiple ADD clauses (Oracle allows this, was failing)
ALTER TABLE schema.table
    ADD col1 NUMBER
    ADD col2 VARCHAR(50)
/

-- Another ALTER TABLE ADD with multiple ADD clauses
ALTER TABLE schema.table
    ADD col3 NUMBER
    ADD col4 NUMBER
/

-- Comments with dots in them (package names, should be ignored)
--select rowid, A.* from schema.table A where col1 = 'value'
/

-- Comments with DELETE statements (should be ignored)
--delete from schema.table WHERE col1 = 'value'
/

-- Comments with DROP TABLE statements (should be ignored)
--DROP TABLE schema.table
/

-- ALTER TABLE DROP COLUMN with longer schema names (was failing)
ALTER TABLE schema_suffix.table_name DROP COLUMN col_name
/

-- DROP TABLE with schema.table format in comments (should be ignored)
--DROP TABLE schema_suffix.table_name
/

-- Test cases for reported errors

-- GRANT with multiple users (comma-separated)
GRANT SELECT ON schema.table TO user1, user2
/

-- GRANT with multiple privileges and multiple users
GRANT SELECT, UPDATE ON schema.table TO user1, user2, user3
/

-- ALTER TABLE MODIFY with NULL (no type specified)
ALTER TABLE schema.table MODIFY column_name NULL
/

-- ALTER TABLE MODIFY with NULL (multiple columns)
ALTER TABLE schema.table MODIFY col1 NULL, MODIFY col2 NULL
/

-- ALTER TABLE MODIFY with NULL in parentheses
ALTER TABLE schema.table MODIFY( column_name NULL )
/

-- ALTER TABLE DROP CONSTRAINT
ALTER TABLE schema.table DROP CONSTRAINT constraint_name
/

-- RENAME statement (standalone, not ALTER TABLE)
RENAME old_table TO new_table
/

-- RENAME with schema.table
RENAME schema.old_table TO schema.new_table
/

-- ALTER TABLE ADD with parentheses (single column)
ALTER TABLE schema.table ADD( column_name VARCHAR2(50 BYTE) )
/

-- ALTER TABLE ADD with parentheses (multiple columns)
ALTER TABLE schema.table ADD( col1 NUMBER, col2 VARCHAR2(50) )
/

-- Empty statement with just terminator
/

-- GRANT with WITH GRANT OPTION
GRANT SELECT ON schema.table TO user1 WITH GRANT OPTION
/

-- GRANT with multiple privileges and WITH GRANT OPTION
GRANT SELECT, UPDATE ON schema.table TO user1, user2 WITH GRANT OPTION
/

-- CREATE VIEW with FORCE keyword
CREATE OR REPLACE FORCE VIEW test_view AS SELECT * FROM test_table
/

-- CREATE VIEW with FORCE and EDITIONABLE keywords
CREATE OR REPLACE FORCE EDITIONABLE VIEW test_view2 AS SELECT * FROM test_table
/

-- CREATE VIEW with column list
CREATE OR REPLACE FORCE VIEW test_view3 (col1, col2, col3) AS SELECT col1, col2, col3 FROM test_table
/

-- CREATE VIEW with FORCE, EDITIONABLE, and column list
CREATE OR REPLACE FORCE EDITIONABLE VIEW test_view4 (col1, col2) AS SELECT col1, col2 FROM test_table
/

-- Test cases for reported errors

-- UPDATE with function calls (REPLACE function)
UPDATE schema.table SET col1 = REPLACE(col1, ',', ' ')
/

-- UPDATE with function calls (multiple arguments)
UPDATE schema.table SET col1 = REPLACE(col1, ' ', '_')
/

-- SET SCAN OFF (SQL*Plus directive)
SET SCAN OFF
/

-- SET SCAN ON (SQL*Plus directive)
SET SCAN ON
/

-- ALTER TABLE DROP with parentheses and multiple columns
ALTER TABLE schema.table DROP (
  col1,
  col2,
  col3,
  col4
)
/

-- GRANT with Oracle-specific privileges
GRANT ALTER, DELETE, INDEX, INSERT, REFERENCES, SELECT, UPDATE, ON COMMIT REFRESH, QUERY REWRITE, DEBUG, FLASHBACK ON schema.table TO user1
/

-- INSERT with WITH clause before SELECT
INSERT INTO schema.table (col1, col2, col3)
WITH cte AS (
  SELECT col1, col2, col3 FROM other_table
)
SELECT col1, col2, col3 FROM cte
/

-- ALTER TABLE MODIFY with DEFAULT NULL
ALTER TABLE schema.table MODIFY col1 DEFAULT NULL
/

-- ALTER TABLE MODIFY with DEFAULT NULL in parentheses
ALTER TABLE schema.table MODIFY( col1 DEFAULT NULL )
/

-- Test cases for reported errors

-- INSERT with WITH clause and window functions (ROW_NUMBER OVER)
INSERT INTO schema.table (col1, col2, col3)
WITH cte AS (
  SELECT t.*,
         ROW_NUMBER() OVER (PARTITION BY col1, col2 ORDER BY col3) AS rn
  FROM other_table t
)
SELECT col1, col2, rn FROM cte
/

-- REVOKE statement (similar to GRANT)
REVOKE SELECT, UPDATE, DELETE, INSERT ON schema.table FROM user1
/

-- REVOKE with multiple privileges and multiple users
REVOKE SELECT, UPDATE ON schema.table FROM user1, user2
/

-- INSERT with VALUES (simple case)
INSERT INTO schema.table VALUES ('value1', 'value2', 'value3')
/

-- SELECT with window function in subquery
SELECT col1, col2 FROM (
  SELECT col1, col2, ROW_NUMBER() OVER (PARTITION BY col1 ORDER BY col2) AS rn
  FROM schema.table
) WHERE rn = 1
/

-- Window function with PARTITION BY and ORDER BY
SELECT col1, ROW_NUMBER() OVER (PARTITION BY col1, col2 ORDER BY col3, col4) AS rn FROM schema.table
/

-- Window function with complex ORDER BY
SELECT col1, ROW_NUMBER() OVER (PARTITION BY col1 ORDER BY col2 DESC, col3 ASC) AS rn FROM schema.table
/

-- INSERT with WITH clause containing UNION ALL
INSERT INTO schema.table (col1, col2)
WITH cte AS (
  SELECT col1, col2 FROM table1 WHERE condition = 'value'
  UNION ALL
  SELECT col1, col2 FROM table2 WHERE condition = 'value'
)
SELECT col1, col2 FROM cte
/

-- Test cases for reported errors

-- CREATE VIEW with multi-line column list and UNION ALL (was failing at AS)
CREATE OR REPLACE FORCE VIEW test_view_union (
  col1,
  col2,
  col3
)
AS
  SELECT "col1", "col2", "col3" FROM table1
  UNION ALL
  SELECT "col1", "col2", "col3" FROM table2
/

-- CREATE VIEW with quoted identifiers in column list and UNION ALL
CREATE OR REPLACE FORCE VIEW test_view_quoted (
  "COL1",
  "COL2"
)
AS
  SELECT "COL1", "COL2" FROM table1
  UNION ALL
  SELECT "COL1", "COL2" FROM table2
/

-- CREATE VIEW with multi-line column list (no UNION, just to test column list parsing)
CREATE OR REPLACE FORCE VIEW test_view_multiline (
  col1,
  col2,
  col3,
  col4
)
AS
  SELECT col1, col2, col3, col4 FROM test_table
/

-- Test cases for reported errors

-- PL/SQL block: BEGIN ... END
BEGIN
  NULL;
END;
/

-- PL/SQL block: FOR ... LOOP ... END LOOP
BEGIN
  FOR rec IN (SELECT 'test' AS cmd FROM DUAL) LOOP
    NULL;
  END LOOP;
END;
/

-- INSERT INTO table VALUES without column list
INSERT INTO test_table VALUES ('value1', 'value2', 'value3')
/

-- INSERT INTO table VALUES with subquery in VALUES
INSERT INTO test_table VALUES ('value1', (SELECT MAX(id) FROM other_table), 'value3')
/

-- INSERT INTO table VALUES with multiple rows and subquery
INSERT INTO test_table VALUES ('value1', (SELECT MAX(id) FROM other_table), 'value3'), ('value2', 100, 'value4')
/

-- CREATE TABLE with semicolon terminator (not slash)
CREATE TABLE test_semicolon (
  id NUMBER PRIMARY KEY,
  name VARCHAR2(50)
);
/

-- CREATE TABLE with DEFAULT SYSDATE NOT NULL
CREATE TABLE test_default_sysdate (
  id NUMBER PRIMARY KEY,
  created_at DATE DEFAULT SYSDATE NOT NULL,
  updated_at DATE DEFAULT SYSDATE NOT NULL
)
/

-- CREATE TABLE with DEFAULT SYSDATE followed by NOT NULL on separate lines
CREATE TABLE test_default_sysdate_multiline (
  id NUMBER PRIMARY KEY,
  created_at DATE DEFAULT SYSDATE
    NOT NULL,
  updated_at DATE DEFAULT SYSDATE
    NOT NULL
)
/

-- CREATE TABLE with schema.table and DEFAULT SYSDATE NOT NULL
CREATE TABLE schema.test_table (
  id NUMBER PRIMARY KEY,
  created_at DATE DEFAULT SYSDATE NOT NULL
)
/

-- INSERT INTO schema.table VALUES with subquery
INSERT INTO schema.test_table VALUES ('value1', (SELECT MAX(id) FROM schema.other_table), SYSDATE)
/

-- Test cases for standalone NULL annotations (Oracle allows explicit NULL)

-- CREATE TABLE with NUMBER columns having explicit NULL annotation
CREATE TABLE schema.test_null_annotations (
  col1 NUMBER(19,2) NULL,
  col2 NUMBER(19,2) NULL,
  col3 NUMBER(19,6) NOT NULL
)
/


Some random text that should fail to parse.
/

-- CREATE TABLE with VARCHAR2 column having explicit NULL annotation
CREATE TABLE schema.test_varchar_null (
  col1 VARCHAR2(1024 BYTE) NULL,
  col2 VARCHAR2(50 BYTE) NOT NULL
)
/

-- CREATE TABLE with DATE DEFAULT SYSDATE NOT NULL (DEFAULT before NOT NULL)
CREATE TABLE schema.test_default_not_null (
  col1 DATE DEFAULT SYSDATE NOT NULL,
  col2 DATE DEFAULT SYSDATE NULL
)
/

-- CREATE TABLE with TIMESTAMP precision and NULL annotation
CREATE TABLE schema.test_timestamp_null (
  col1 TIMESTAMP(6) NULL,
  col2 TIMESTAMP(6) NOT NULL,
  col3 TIMESTAMP NULL
)
/

-- CREATE TABLE with CHAR and NULL annotation
CREATE TABLE schema.test_char_null (
  col1 CHAR(1 BYTE) NULL,
  col2 CHAR(1 BYTE) NOT NULL
)
/

/* Some block comment */

COMMENT ON COLUMN schema.test_char_null.col1 IS 'Name of column';

ALTER TABLE
  schema.test_table
ADD
  CONSTRAINT UNIQ_TBL_NAME UNIQUE (col1, col2);

-- ALTER TABLE with multiple schemas
ALTER TABLE schema.config_table ADD CONSTRAINT uq_section_param UNIQUE (section_id, param_name);
ALTER TABLE schema.user_perms ADD CONSTRAINT uq_user_app UNIQUE (user_name, app_name);
-- Comment with constraint syntax
--  constraint UNIQ_SET unique (app_name, table_name, user_name);
/

DROP TABLE schema.product_scope PURGE;
GRANT SELECT ON aggregate_view TO read_user;
/

-- INSERT with block comment in column list
INSERT INTO schema.app_config (app_name, app_desc, settings/*, validation_action*/)
VALUES ('TST', 'Test App','{ "enabled" : true }'/*,
'com.example.validators.AppValidator'*/);
/

-- INSERT with inline comment and block comment spanning multiple lines
INSERT INTO schema.column_def (app_name, col_name, col_order/*, col_validator*/) VALUES
('TST', 'col_code', 1/*,
'com.example.validators.RegexValidator:^[a-zA-Z0-9_.-]*$'*/);
/

-- CREATE INDEX tests
CREATE INDEX idx_emp_id ON employees(emp_id);

CREATE UNIQUE INDEX idx_emp_email ON employees(email);

CREATE INDEX idx_composite ON employees(dept_id, hire_date);


-- ALTER TABLE ADD CONSTRAINT FOREIGN KEY tests
ALTER TABLE orders ADD CONSTRAINT fk_orders_customer FOREIGN KEY (customer_id) REFERENCES customers(id);

ALTER TABLE orders ADD CONSTRAINT fk_orders_product FOREIGN KEY (product_id) REFERENCES products(id) ON DELETE CASCADE;

ALTER TABLE orders ADD CONSTRAINT fk_orders_shipper FOREIGN KEY (shipper_id) REFERENCES shippers(id) ON DELETE SET NULL ON UPDATE CASCADE;

-- Test: String literals containing '/' should not be split as statement terminators
ALTER TABLE test_table MODIFY(col1 DEFAULT ON NULL 'N/A');

-- Test: DROP TABLE with CASCADE CONSTRAINTS
DROP TABLE schema.test_table CASCADE CONSTRAINTS;

ALTER TABLE orders MODIFY shipper_id NOT NULL ENABLE VALIDATE;

commit;
