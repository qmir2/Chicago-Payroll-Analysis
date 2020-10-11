//
// F# program to analyze payroll data.
//
// Qasim Mir
// U. of Illinois, Chicago
// CS 341, Spring 2020
// Project #02
//

module Project02


//
// doubleOrNothing
//
// Given a string containing a double numeric value
// or being an empty string
// returns the double equivalent, with the empty string
// treated as the value 0.0
//
let doubleOrNothing s = 
    match s with
    | "" -> 0.0
    | x -> double x

//
// ParseCSVLine and ParseCSVDatabase
//
// Given a sequence of strings representing payroll data, 
// parses the strings and returns a list of tuples.  Each
// sub-list denotes one employee.  Example:
//
//[ ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0); ... ]
//
// The values are first name, last name, occupation,
// department, salary type, hours per week, annual salary,
// and hourly wage. 
// Depending on the salary type, 
// either hours per week and hourly wage 
// or annual salary will be filled in with 0.0,
// since the field is empty in the csv
//
// First Name, Last Name, Occupation, Dept Name, Fulltime or Part time, Typical Hours, Annual Salary, Hourly Rate
let ParseCSVLine (line:string) = 
    let tokens = line.Split(',')
    let listOfValues = Array.toList tokens
    //match listOfValues with  // alternative implmentation to remove warnings
    //| fName::lName::occupation::department::salaryType
    //    ::hoursPerWeek::annualSalary::hourlyWage::[] -> (fName,lName,occupation,department,salaryType,
    //                                                      (doubleOrNothing hoursPerWeek),
    //                                                      (doubleOrNothing annualSalary),
    //                                                      (doubleOrNothing hourlyWage)
    //                                                    )
    //| _ -> failwith "Insufficient values in line of csv file"

    let fName::lName::Occupation::Department::SalaryType
        ::HoursPerWeek::AnnualSalary::HourlyWage::[] = listOfValues
    (fName,lName,Occupation,Department,SalaryType,
      (doubleOrNothing HoursPerWeek),
      (doubleOrNothing AnnualSalary),
      (doubleOrNothing HourlyWage)
    )

let rec ParseCSVDatabase lines = 
    let employees = Seq.map ParseCSVLine lines
    //printfn "%A" employees
    Seq.toList employees


// Returns whether the employee is salaried
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// true
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// false 
//
let isSalary employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    SalaryType = "Salary"

// Returns whether the employee is paid hourly, not salaried
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// false
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// true 
//
let isHourly employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    SalaryType = "Hourly"

// Returns the full name of the employee
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// "JESSE A ACOSTA"
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// "JOHN R KEATING"
//
let getName employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    fName + " " + lName

// Returns name of the department the employee belongs to
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// "POLICE"
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// "GENERAL SERVICES" 
//        
let getDepartment employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    Department

//
//The function calcSalary calculates the annual salary, 
// either by directly returning the recorded annual salary 
// or calculating it by finding the average weekly salary 
// by multiplying the hours per week by hourly wage, 
// and then multiplying that by 52 to get the average 
// annual salary for that hourly worker.    
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// 93354.0
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// 52.0*40.0*52.35 = 108888.0 
//
let calcSalary employee =
    let (fName,lName,Occupation,Department,SalaryType,
         HoursPerWeek, AnnualSalary, HourlyWage
        ) = employee
    if SalaryType = "Salary" then AnnualSalary //if salarytype is salary then print that
    else 52.0*HoursPerWeek*HourlyWage //calculate salary from hourlywage
        


// The function getNumberOfEmployees returns the number of employees in the data set.
let getNumberOfEmployees allData =
    List.length allData 

// The function getNumberOfSalariedEmployees returns the number of employees who have an annual salary in the data set.
let getNumberOfSalariedEmployees allData =
    List.length (List.filter (fun x -> isSalary x) allData) //first filter out employee by salary then find length


// The function getNumberOfSalariedEmployees returns the number of employees who are paid hourly in the data set.
let rec getNumberOfHourlyEmployees allData =
   List.length (List.filter (fun x -> isHourly x) allData) //first filter out employee by hourly then find length

// The function findHighestPaidEmployee returns the name and salary
// of the highest paid employee.
// Use the computed salary for hourly employees.

let rec helperHighestPaidAll tuple allData =
    match allData with
    |[] -> tuple //return the last 
    |hd::tl -> if calcSalary(hd) > calcSalary(tuple) then helperHighestPaidAll hd tl //if hd is greater than previous salary then replace previous with hd 
               else helperHighestPaidAll tuple tl //otherwise recurse to next
    
let rec findHighestPaidEmployee allData =
    let x = helperHighestPaidAll (List.head allData) (List.tail allData) //call the helper
    (getName x, calcSalary x) //at the end you get the tuple of name and salary of highest paid employee

// The function findHighestPaidEmployee returns the salary
// of the highest paid employee within a specific department.
// Use the computed salary for hourly employees.
let rec findHighestPaidEmployeeInDept allData deptName =
   snd(findHighestPaidEmployee(List.filter(fun x -> deptName = getDepartment x) allData)) //filter out by department then call the highest paid employee function then get the second

// The function getAverageSalary calculates
// the average of the computed salary field.
let getAverageSalary allData =
    List.average (List.map(fun x -> calcSalary x) allData) //make a list of salaries and then compute the average
    

// The function getAverageSalary calculates 
// the average of the computed salary field for a specific department.
let getAverageSalaryInDept allData deptName =
    getAverageSalary(List.filter(fun x -> deptName = getDepartment x) allData) //filter by your specific department and then call the average function created above

// Searches through the data set to generate the list of all unique department names.
let getUniqueDepartmentNames allData =
    allData
    |> List.map getDepartment
    |> List.distinct
    |> List.sort
    //without piping
    //let allDepts = List.map getDepartment allData
    //let uniqueDepts = List.distinct allDepts
    //uniqueDepts


// The function howManyEmployeesInEachDepartment computes the number of employees in every department.  
// This function should return a list of tuples, pairs between the department name and number of employees. 
let helperInEachDept allData deptNames =
    List.length(List.filter (fun x -> deptNames = getDepartment x) allData)

let rec howManyEmployeesInEachDepartment allData deptNames =
    match deptNames with
    |[] -> [] //if empty then empty list
    |hd::tl -> (hd, helperInEachDept allData hd)::howManyEmployeesInEachDepartment allData tl 
    //so you want to prepend the the tuple with hd(department) as your first elem and the length called from the helper as your second elem


// The function findTotalSalaryByDepartment computes the overall annual salary budget for every department. 
// The calculated salary should include the average annual salary for hourly employees.
// This function should return a list of tuples, pairs between the department name and total annual salary. 
let helperTotalSalDep allData deptNames =
    let list = List.filter (fun x -> deptNames = getDepartment x) allData
    List.sum(List.map(fun x -> calcSalary x) list)

let rec findTotalSalaryByDepartment allData deptNames =
    match deptNames with
    |[] -> []
    |hd::tl -> (hd, helperTotalSalDep allData hd)::findTotalSalaryByDepartment allData tl
    //so you want to prepend the the tuple with hd(department) as your first elem and the sum of salaries called from the helper as your second elem



// The function findHighestPaidDeptOverall returns the name and total annual salary
// of the department with the largest overall annual salary paid to employees in that department. 
// The calculated salary should include the average annual salary for hourly employees.
// This function should return a single tuple, containing the department name and total annual salary. 

let rec helperHighestPaidDeptOverall tuple list  =
    match list with
    |[] -> tuple 
    |hd::tl -> if snd(hd) > snd(tuple) then helperHighestPaidDeptOverall hd tl // if salary of the hd is greater than the previous number then replace it
               else helperHighestPaidDeptOverall tuple tl //otherwise keep the salary and go to next

       
let rec findHighestPaidDeptOverall allData deptNames =
    let list = findTotalSalaryByDepartment allData deptNames //make a list of tuples of department and salaries
    helperHighestPaidDeptOverall ("Nobody", 0.0) list //call your helper

// The function withinSalaryRange returns the number of employees whose calculated salary
// is greater than the lower bound and less than or equal to the upper bound.
let rec withinSalaryRange lower upper L =
    List.length (List.filter (fun x -> calcSalary(x)>lower && calcSalary(x)<=upper) L) 

//
// printOneHistogram
//
// prints one histogram value, the format is
//   label: *****value
// where the # of stars is value / amountPerStar.
//
let printOneHistogram label value amountPerStar =
  // 
  // helper function to print # of stars:
  //
  let rec printstars n = 
    match n with
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*"
           printstars (n-1)
  //
  // print label, stars, and histogram value:s
  //
  printf " %16s: " label    // Enough space for all the departments in the file
  printstars (value / amountPerStar)
  printfn "%A" value




[<EntryPoint>]
let main argv =
    //
    // input file name, then input employee data and build
    // a list of tuples:
    //
    //("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
    //("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
    //let salary = calcSalary("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
    //printfn "%A" salary

    printf "Enter name of the csv file containing employee data: "

    let filename = System.Console.ReadLine() //"payroll_02.csv" //
    let contents = System.IO.File.ReadLines(filename)
    let data = ParseCSVDatabase contents
    //let data = [List.head data]

    //printfn "This is the data you have loaded."
    //List.iter (printfn "%A") data

    printf "Enter name of the department to be analyzed: "

    let deptname = System.Console.ReadLine()
    let allDepts = getUniqueDepartmentNames data

    let N = getNumberOfEmployees data
    printfn ""
    printfn "# of employees: %A" N
    printfn ""

    let deptNs = howManyEmployeesInEachDepartment data allDepts
    //printfn ""
    printfn "# of employees in %s: %d" deptname (helperInEachDept data deptname)  // Figure out how to get the number for one department
    printfn ""

    //
    // % of employees salaried:
    //
    let numSalaried = getNumberOfSalariedEmployees data
    let percentSalaried = (double numSalaried) / (double N) * 100.0
    let numHourly = getNumberOfHourlyEmployees data
    let percentHourly = (double numHourly) / (double N) * 100.0

    printfn "%% of employees Salaried: %d (%.2f%%)" numSalaried percentSalaried
    printfn "%% of employees Hourly: %d (%.2f%%)" numHourly percentHourly
    printfn ""

    //
    // average salary:
    //
    let avgSalary = getAverageSalary data
    printfn "Average salary: %.2f" avgSalary
    //printfn ""

    //
    // average salary in department:
    //
    let avgSalary = getAverageSalaryInDept data deptname
    printfn "Average salary in %s: %.2f" deptname avgSalary
    printfn ""
    
    //
    // highest salary:
    //
    let (maxName,maxSalary) = findHighestPaidEmployee data
    printfn "Largest salary: %s paid %.2f annually" maxName maxSalary
    //printfn ""

    //
    // highest salary in department:
    //
    let maxSalary = findHighestPaidEmployeeInDept data deptname
    printfn "Largest salary in %s: %.2f" deptname maxSalary
    printfn ""

    printfn "** Histogram of employees by department (each star represents 5 employees):"    
    // Use printOneHistogram to build the histogram

    deptNs |> List.iter (fun (x,y) -> printOneHistogram x y 5) //print the histogram  of employees by department

    printfn ""
    //
    // categorize salaries into 5 groups:
    //   0       < salary <= 60000
    //   60000   < salary <= 80000
    //   80000   < salary <= 100000
    //   100000  < salary <= 120000
    //   120000  < salary <= 10,000,000  // arbitrary upper bound to reuse function
    //

    //let u = withinSalaryRange 60000.0 80000.0 data
    //printfn "OUTPUT %A" u

    let count60korless = withinSalaryRange 0.0 60000.0 data
    let percent60korless = (double count60korless) / (double N) * 100.0

    let count60kto80k = withinSalaryRange 60000.0 80000.0 data
    let percent60kto80k = (double count60kto80k) / (double N) * 100.0

    let count80kto100k = withinSalaryRange 80000.0 100000.0 data
    let percent80kto100k = (double count80kto100k) / (double N) * 100.0

    let count100kto120k = withinSalaryRange 100000.0 120000.0 data
    let percent100kto120k = (double count100kto120k) / (double N) * 100.0

    let countgreater120k = withinSalaryRange 120000.0 10000000.0 data
    let percentgreater120k = (double countgreater120k) / (double N) * 100.0

    printfn "** Salary Ranges:"
    printfn " 0-60000 : %A (%.2f%%)" count60korless percent60korless
    printfn " 60000-80000 : %A (%.2f%%)" count60kto80k percent60kto80k
    printfn " 80000-100000: %A (%.2f%%)" count80kto100k percent80kto100k
    printfn " 100000-120000: %A (%.2f%%)" count100kto120k percent100kto120k
    printfn " > 120000: %A (%.2f%%)" countgreater120k percentgreater120k
    printfn ""

    printfn "** Histogram of Salary Ranges (each star represents 10 employees):"    
    let salaryGroups = [("<60000", count60korless);("60-80k", count60kto80k);("80-100k", count80kto100k);("100-120k", count100kto120k);(">120000",countgreater120k)]
    // Use printOneHistogram to build the histogram

    salaryGroups |> List.iter (fun (x,y) -> printOneHistogram x y 10) //prints the histogram of salary
    
    0    
