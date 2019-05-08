# Create publication table of 20 variables
library(hips)

targets_df <- hipsInfo(targets)

# Variable descriptions
descriptions <- c(
    fx = "radiologist documented fracture in the impression report (abstracted with NLP)",
    age = "patient age (years)",
    sex = "patient's gender",
    bmi = "patient's body mass index (kg/m2)",
    fall_lgl = "clinical history of patient falling (abstracted from radiologist's report with regex)",
    pain_lgl = "clinical history of patient reporting pain (abstracted from radiologist's report with regex)",
    dept = "hospital setting",
    name_operator = "technician who acquired the radiograph",
    name_interpreter = "radiologist who interpreted the radiograph",
    device_brand = "company that manufactured the scanner (included in dicom header)",
    device_model = "device that acquired the radiograph",
    dT.exam_to_prelim = "wait time between image acquisition and the initial interpretation (hours)",
    dT.exam_to_final = "wait time between image acquisition and the final interpretation (hours)",
    order_date = "study day that the image was acquired (days since first scan acquired)",
    order_wday = "day of week of 'Order Date'",
    dT.ordered_to_exam = "wait time between image order and image acquisition (hours)",
    projection_set = "image views ordered",
    procedure_radiation = "dose of radiation used (uAs)",
    order_time = "time the image was ordered",
    order_priority = "whether the order was routine or urgent"
)

DF <- descriptions %>%
    tibble::enframe() %>%
    right_join(targets_df, by = c("name" = "target"))


DF %<>% `[`(, c("name", "target_mode", "value"))


#! TODO: Add columns for "representation" and "distribution"


names(DF) <- c("Variable", "Class", "Description")
DF$Variable %<>% mapvalues(from = names(hips:::AES_TARGET_LABELS), to = hips:::AES_TARGET_LABELS)

Write(DF, fp = "inst/info/targets.csv")
