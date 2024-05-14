```sql
SELECT
BIT_AND(age) AS age_and
FROM UNNEST([32, 45, 28, 32]) AS age;
```