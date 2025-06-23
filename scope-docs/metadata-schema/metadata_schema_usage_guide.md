# Metadata Schema Usage Guide

This guide provides practical guidance for using the recodeflow metadata schema system.

## Validation Workflow

### How validation tools should use these specifications:

- **CSV import validation**: Use `shared_specifications` from metadata_registry.yaml for format requirements and validation rules
- **Schema validation**: Use individual schema files (variables.yaml or variable_details.yaml) for field-specific validation
- **Cross validation**: Check variables referenced in variable_details exist in variables.csv

## File Relationships

### How the three files work together:

1. **Start with registry**: Begin with metadata_registry.yaml to understand shared specifications
2. **Schema-specific details**: Use variables.yaml or variable_details.yaml for detailed field requirements
3. **Database extensions**: Use database-specific files (e.g., variables_cchs_example.yaml) for project customizations

## Task-Specific Usage

- **CSV validation**: metadata_registry.yaml + relevant schema file
- **Layout assessment**: relevant schema file (variables.yaml or variable_details.yaml)
- **Extension discovery**: metadata_registry.yaml extension_registry section
- **Pattern validation**: Use transformation_patterns and interval_notation from registry

## Cross-Validation Rules

### Variable references
- **Description**: Variables in variable_details must exist in variables.csv
- **Validation**: Check variable_details.variable against variables.variable

### Database consistency
- **Description**: Database references should be consistent
- **Validation**: Check databaseStart values match across files

### Template references
- **Description**: Template usage must reference existing template definitions
- **Validation**: templateVariable values must reference defined templates or be 'Yes'/'No'

## Implementation Examples

### dummyVariable Creation

**Categorical variables:**
```
For age categories: age_cat4_1, age_cat4_2, age_cat4_3, age_cat4_4
Pattern: {variable}_{cat|cont}{num_categories}_{category_number}
```

**Continuous variables:**
```
For BMI: bmi_cont1, height_cont1, weight_cont1
Pattern: {variable}_{cat|cont}{transformation_number}
```

### variableStart Usage Patterns

- **Simple**: `[HEIGHT]` - references HEIGHT variable from any database
- **Database specific**: `cchs2017_p::HWT_2` - specific database and variable
- **Derived**: `DerivedVar::[HEIGHT_CM, WEIGHT_KG]` - calculated from multiple variables
- **Complex**: `cchs2001_p::VAR1, cchs2003_p::VAR2, [VAR3]` - mixed sources

### recStart Patterns

- **Categorical**: `English, French, 1, 2, NA::a`
- **Numeric ranges**: `[18.5,24.9], (0,18), [-0.359,1]`
- **Missing data**: `NA::a (valid skip), NA::b (don't know/refusal)`

## Working with Database-Specific Extensions

### CCHS Example
- Use `variables_cchs_example.yaml` and `variable_details_cchs_example.yaml` as templates
- Follow validation rules defined in these files
- Maintain version consistency (CCHS extensions use v2.2.0+)

### Creating New Database Extensions
1. Create `variables_{database}_example.yaml` and `variable_details_{database}_example.yaml`
2. Reference `registry_file: "metadata_registry.yaml"` in both files
3. Define database-specific validation rules and field extensions
4. Add entry to `supported_databases` section in metadata_registry.yaml