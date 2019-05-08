#' @get /ntm/<proposal_id>/<account_id>

function(proposal_id, account_id) {
  
  options(scipen = 200, 
          mongodb = list(
            "host" = "mongodb://localhost:27017", 
            "db" = "pharbers-ntm-client"
          ), 
          digits = 13, 
          digits.secs = 3)
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(mongolite)
  library(jsonlite)
  
  print(getwd())
  
  source("./Functions.R", encoding = "UTF-8")
  
  # proposal_id <- "5cc018a2f4ce4374c23cece6"
  # account_id <- "5c4552455ee2dd7c36a94a9e"
  
  if (!missing(account_id) & !missing(proposal_id)) {
    
    db_paper <- mongo(collection = "Paper", db = options()$mongodb$db, url = options()$mongodb$host)
    paper_info <- db_paper$find(query = paste0('{"proposal-id" : "', proposal_id, '", "account-id" : "', account_id, '"}'))
    p_sales_report_id <- paper_info$`sales-report-ids`[[1]][4]
    personnel_assessment_id <- paper_info$`personnel-assessment-ids`[[1]][1]
    
    db_input <- mongo(collection = "Paperinput", db = options()$mongodb$db, url = options()$mongodb$host)
    input_ids <- paper_info$`input-ids`[[1]]
    input_info <- data.frame()
    for(i in input_ids) {
      info <- db_input$find(query = paste0('{"_id" : {"$oid" : "', i, '"}}'), fields = '{"_id" : 1, "time" : 1}')
      input_info <- bind_rows(input_info, info)
    }
    input_id <- input_info$`_id`[which.max(input_info$time)]
    
    db_paper_input <- mongo(collection = "Paperinput", db = options()$mongodb$db, url = options()$mongodb$host)
    phase_info <- db_paper_input$find(query = paste0('{"_id" : {"$oid" : "', input_id, '"}}'), fields = '{"_id" : 1, "phase" : 1}')
    phase <- phase_info$phase
    
    curves <- get_intermedia(uuid = "intermedia", type = "curves")
    weightages <- get_intermedia(uuid = "intermedia", type = "weightages")
    
    p_data <- get_p_data(proposal_id = proposal_id, p_sales_report_id = p_sales_report_id, personnel_assessment_id = personnel_assessment_id)
    input_data <- get_input_data(input_id = input_id)
    
    dat <- get_data2use(p_data = p_data, input_data = input_data)
    
    ## results
    results <- get_results(dat = dat, curves = curves, weightages = weightages)
    
    ## reports
    hosp_report <- get_hosp_report(results = results)
    rep_report <- get_rep_report(results = results)
    prod_report <- get_prod_report(results = results, p_sales_report_id = p_sales_report_id)
    
    ## output reports
    proposal_id <- paper_info$`proposal-id`
    paper_input_ids <- paper_info$`input-ids`[[1]]
    paper_input_id <- paper_input_ids[length(paper_input_ids)]
    
    db_scenario <- mongo(collection = "Scenario", db = options()$mongodb$db, url = options()$mongodb$host)
    scenario_info <- db_scenario$find(query = paste0('{"proposal-id" : "', proposal_id, '", "phase" : ', format(phase, nsmall = 1), '}'), fields = '{}')
    scenario_id <- scenario_info$`_id`
    
    db_hosp_report <- mongo(collection = "HospitalSalesReport", db = options()$mongodb$db, url = options()$mongodb$host)
    
    db_hosp_report$insert(hosp_report, 
                          na = "string", 
                          auto_unbox = TRUE)
    
    hospital_sales_report_ids <- tail(db_hosp_report$find(query = '{}', fields = '{"_id" : 1}'), nrow(hosp_report))$`_id`
    
    db_rep_report <- mongo(collection = "RepresentativeSalesReport", db = options()$mongodb$db, url = options()$mongodb$host)
    
    db_rep_report$insert(rep_report, 
                         na = "string", 
                         auto_unbox = TRUE)
    
    representative_sales_report_ids <- tail(db_rep_report$find(query = '{}', fields = '{"_id" : 1}'), nrow(rep_report))$`_id`
    
    db_prod_report <- mongo(collection = "ProductSalesReport", db = options()$mongodb$db, url = options()$mongodb$host)
    
    db_prod_report$insert(prod_report, 
                          na = "string", 
                          auto_unbox = TRUE)
    
    product_sales_report_ids <- tail(db_prod_report$find(query = '{}', fields = '{"_id" : 1}'), nrow(prod_report))$`_id`
    
    db_sales_report <- mongo(collection = "SalesReport", db = options()$mongodb$db, url = options()$mongodb$host)
    
    db_sales_report$update(query = paste0('{"time" : "0"}'), 
                           update = paste0('{"$set" : {"scenario-id" : ', toJSON(scenario_id, auto_unbox = TRUE), 
                                           ', "hospital-sales-report-ids" : ', toJSON(as.list(hospital_sales_report_ids), auto_unbox = TRUE), 
                                           ', "representative-sales-report-ids" : ', toJSON(as.list(representative_sales_report_ids), auto_unbox = TRUE), 
                                           ', "product-sales-report-ids" : ', toJSON(as.list(product_sales_report_ids), auto_unbox = TRUE), 
                                           ', "paper-input-id" : ', toJSON(paper_input_id, auto_unbox = TRUE), 
                                           ', "time" : ', round(as.numeric(Sys.time())*1000), '}}'), 
                           upsert = TRUE)
    
    ## update information
    personnel_assessment_ids <- paper_info$`personnel-assessment-ids`[[1]]
    db_personnel_assessment <- mongo(collection = "PersonnelAssessment", db = options()$mongodb$db, url = options()$mongodb$host)
    personnel_assessment_info <- db_personnel_assessment$find(query = paste0('{"_id" : {"$oid" : "', personnel_assessment_ids[1], '"}}'))
    p_rep_ability_ids <- personnel_assessment_info$`representative-ability-ids`[[1]]
    p_action_kpi_ids <- personnel_assessment_info$`action-kpi-ids`[[1]]
    
    db_rep_ability <- mongo(collection = "RepresentativeAbility", db = options()$mongodb$db, url = options()$mongodb$host)
    db_action_kpi <- mongo(collection = "ActionKpi", db = options()$mongodb$db, url = options()$mongodb$host)
    
    p_action_kpi <- data.frame()
    for (i in 1:length(p_action_kpi_ids)) {
      info <- db_action_kpi$find(query = paste0('{"_id" : {"$oid" : "', p_action_kpi_ids[i], '"}}'))
      p_action_kpi <- bind_rows(p_action_kpi, info)
    }
    
    rep_ability <- get_rep_ability(results = results)
    action_kpi <- get_action_kpi(p_action_kpi = p_action_kpi, rep_ability = rep_ability)
    
    db_rep_ability$insert(rep_ability, 
                          na = "string", 
                          auto_unbox = TRUE)
    
    rep_ability_ids <- tail(db_rep_ability$find(query = '{}', fields = '{"_id" : 1}'), nrow(rep_ability))$`_id`
    
    db_action_kpi$insert(action_kpi, 
                         na = "string", 
                         auto_unbox = TRUE)
    
    action_kpi_ids <- tail(db_action_kpi$find(query = '{}', fields = '{"_id" : 1}'), nrow(action_kpi))$`_id`
    
    db_personnel_assessment$update(query = paste0('{"time" : "0"}'), 
                                   update = paste0('{"$set" : {"representative-ability-ids" : ', toJSON(rep_ability_ids, auto_unbox = TRUE), 
                                                   ', "action-kpi-ids" : ', toJSON(action_kpi_ids, auto_unbox = TRUE), 
                                                   ', "paper-input-id" : ', toJSON(paper_input_id, auto_unbox = TRUE), 
                                                   ', "scenario-id" : ', toJSON(scenario_id, auto_unbox = TRUE), 
                                                   ', "time" : ', round(as.numeric(Sys.time())*1000), '}}'), 
                                   upsert = TRUE)
    
    ## update paper
    sales_report_ids <- paper_info$`sales-report-ids`[[1]]
    sales_report_ids_info <- db_sales_report$find(query = paste0('{"scenario-id" : "', scenario_id, '", "paper-input-id" : "', paper_input_id, '"}'), fields = '{"_id" : 1, "time" : 1}')
    sales_report_id_new <- sales_report_ids_info$`_id`[which.max(sales_report_ids_info$time)]
    personnel_assessment_ids_info <- db_personnel_assessment$find(query = paste0('{"scenario-id" : "', scenario_id, '", "paper-input-id" : "', paper_input_id, '"}'), fields = '{"_id" : 1, "time" : 1}')
    personnel_assessment_id_new <- personnel_assessment_ids_info$`_id`[which.max(personnel_assessment_ids_info$time)]
    
    db_paper$update(query = paste0('{"proposal-id" : "', proposal_id, '", "account-id" : "', account_id, '"}'), 
                    update = paste0('{"$set" : {"sales-report-ids" : ', toJSON(c(sales_report_ids, sales_report_id_new), auto_unbox = TRUE), 
                                    ', "personnel-assessment-ids" : ', toJSON(c(personnel_assessment_ids, personnel_assessment_id_new), auto_unbox = TRUE), '}}'), 
                    upsert = FALSE)
    
    ## output
    return(list(status = unbox("Success")))
    
  } else {
    
    return(list(status = unbox("Failed")))
  }
}






























