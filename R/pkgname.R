#' Integrate pharmacometric inputs, outputs, and metadata.
#'
#' \pkg{pharmval} is a dataset design that supports inputs, outputs, and associated metadata for pharmacometric analyses.  It extends the generalized dataset design of Schmidt and Radivojevic (2014) Table 3 (\url{http://dx.doi.org/10.1007/s10928-014-9370-4}).
#'
#' The paradigm invokes the concept of a pharmacometric analysis in the limited sense: i.e., the fitting of a model.  Such an analysis typically has associated values that are inputs or outputs, tabular or singular, subject-specific or system-wide, qualitative or quantitative. These values may be grouped as named properties, while the properties themselves may have associated metadata that is useful for interpretation.  The goal of \pkg{pharmval} is to integrate the data and metadata pertaining to a single analysis for efficient storage and manipulation.
#'
#' \pkg{pharmval} builds on the design of Schmidt and Radivojevic (2014) by adding the record-level data items REPEATED, EXCLUDED, and IMPUTED (while renaming some other items).  It also proposes usage guidance for encoding metadata and distinguishing data subtypes. When applied, the guidance enables workflow developers to create reliable, reusable tools for processing data and metadata.  Some such tools are implemented in this package.  However, the pharmval design itself intends to be system-neutral.
#'
#' \describe{
#'  \item{1. The key items are VARIABLE, USUBJID, TIME, REPEATED, nested in that order.}{
#'  The key items represent a hierarchical object model that distinguishes particular records.  For instance, the value reported by a record may pertain to the analysis as a whole, or may be a repeated measures observation for a particular subject at a particular time.
#'  }
#'  \item{2. Key values may be missing, except for VARIABLE.}{
#'  There is no point in storing a record or a value if it is not even known what sort of a variable is represented thereby.  However, an analysis-level variable (e.g. minimum value of the objective function) will not have a related subject-level identifer, nor any of the nested keys.  Similarly, a subject-level variable (e.g. sex) will be invariant with respect to time, etc.
#'  }
#'  \item{3. If a key value is missing, all nested key values should be missing.}{
#'  This principle is a consequence of the hierarchical nature of the object model.
#'  }
#'  \item{4. Key missingness should be consistent within value of VARIABLE.}{
#'  Key missingness is a structural artifact of the object model, and of our attempts to describe different sorts of objects in the same table.  Therefore it can and should be consistent within variable.
#'  }
#'  \item{5. Combinations of key values should be unique.}{
#'  The role of key values is to uniquely identify records.  Duplicate keys are symptomatic of incorrect data; either at least one record communicates incorrect observations, or the implied object type has not been adequately distinguished.
#'  }
#'  \item{6. TYPEC and SUBTYPEC may not be missing.}{
#'  TYPEC is a user-defined classification of (the value of) VARIABLE, and SUBTYPE ranks VARIABLE within TYPEC.  TYPEC is not part of the object hierarchy: VARIABLE must have a globally-unique meaning, not merely unique within TYPEC.
#'  }
#'  \item{7. TYPEC and SUBTYPEC must be consistent within value of VARIABLE.}{
#'  TYPEC and SUBTYPEC are, effectively, "properties" of the value of VARIABLE, and are therefore invariant.
#'  }
#'  \item{8. TYPE and SUBTYPE are numeric, and have a one-to-one correspondence with TYPEC and SUBTYPEC, respectively.}{
#'  These give the same nesting information as TYPEC and SUBTYPEC; they are useful for creating default order in a presentation.
#'  }
#'  \item{9. VALUE and VALUEC may be missing.  If VALUEC is defined, VALUE should also be defined.}{
#'  While object-appropriate keys are required for structural reasons, the pharmacometric value itself is not; there may be good reasons to report and describe missing values.
#'
#'  VALUE is numeric and may have no associated character representation.  VALUEC, however, is typically categorical, in which case VALUE is a numeric that supplies a default order.  Even for categorical variables that are not inherently ordered, some choice of order must pertain during presentation, and that choice may be captured in VALUE. Less clear is the case where VALUEC is arbitrary text, rather than categories.
#'  }
#'  \item{10. Values of TYPEC and VARIABLE should conform to SAS variable conventions.}{
#'  The FDA submission standards specify the SAS 5 naming conventions, including limit to 8 characters.  \pkg{pharmval} item names themselves are compliant. TYPEC and VARIABLE values should comply also, as these may be transformed into item names by means of some related workflow (such as creation of a NONMEM dataset).
#'  }
#'  \item{11. 'META' is a reserved value of VARIABLE.}{
#'  The \pkg{pharmval} paradigm relies on this value to distinguish metadata from primary data.
#'  }
#'  \item{12. If VARIABLE is 'META', VALUEC is a pharmval item name, such as VARIABLE.}{
#'  The intent (see below) is to provide metadata about values in the identified column.
#'  }
#'  \item{13. Where VARIABLE is 'META', the effective key is TYPEC. This record is a declaration. All other records with the same value of TYPEC constitute a metadata table.}{
#'  TYPEC holds the name of an attribute of the related values in the specified column. Note that this creates a possible many-to-one relationship between TYPEC and, say, VARIABLE, which is prohibited above. Therefore, such restrictions are understood to apply only to primary (not meta) data records.
#'  }
#'  \item{14. For records of a metadata table, VARIABLE identifies a (possible) value -- the target -- from the column indicated in the declaration; TYPEC identifies an attribute of the target; VALUE and (optionally) VALUEC give the value of the attribute of the target.}{
#'
#'  For example, consider the SAS-compliant VARIABLE value DV.  To supply an informative label for DV, we create a declaration record with TYPEC:LABEL, VARIABLE:DV, VALUEC:VARIABLE. We then create a one-record metadata table with TYPEC:LABEL, VARIABLE:DV, VALUEC:'observed plasma drug concentration'.
#' }
#' \item{15.  A valid pharmval object may have zero records, or only metadata records, or only data records. Declarations may be present without corresponding metadata tables, but not vice versa. }{
#' Declarations are needed to unambiguously distinguish and characterize metadata tables.
#' }
#'}
#'
#' @references Schmidt H and Radivojevic A. 2014. Enhancing population pharmacokinetic modeling efficiency and quality using an integrated workflow. Journal of Pharmacokinetics and Pharmacodynamics 41(4): 319-334.
"_PACKAGE"
