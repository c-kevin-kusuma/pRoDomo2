# pRoDomo2
This R package provides seamless integration between DOMO and R programming language. Advanced analytics and a wide range of process automation are possible and made simple by this package.  

While DOMO is a low-code data app platform that takes the power of BI to the next level, R programming language enables data scientists to develop advanced analysis with unlimited flexibility. Integrating both tools will open new doors to possibilities and opportunities. 

<br />
<br />

## How to Install
Currently, this package is only available through GitHub. You can install this package using devtools github method.
```r
# Checking if "devtools" package is already installed
if(!require("devtools")) install.packages("devtools")

# Installing "pRoDomo2" package from github
devtools::install_github(repo='c-kevin-kusuma/pRoDomo2', ref='main')
```
<br />

## Authentication
This package relies on DOMO's [OAuth authentication](https://developer.domo.com/portal/1845fc11bbe5d-api-authentication), which means `client_id` and `secret` are required. 
Please follow these steps to obtain them:
1. Get a DOMO instance if you don't already have one: for developers use [developer](https://www.domo.com/start/developer) & students use [student](https://www.domo.com/start/student)
2. Go to [developer.domo.com](https://developer.domo.com/login) and enter the domain of your DOMO instance (the subdomain before ".domo.com")
3. Click "Create a client"
4. Give your client a name and select its scope
5. Click "Create", you'll then be provided with `client_id` and `secret`

<br />

## Create A Dataset
`pRoDsCreate()` function lets you create an API dataset from your R environment by leveraging STREAM API that loads data parts at scale.
It will then `print()` the dataset_id of the newly created dataset on the console.
```r
pRoDsCreate(client_id = client_id,
  secret = secret,
  dataset = dataset,
  dataset_name = 'The Name of the Dataset',
  dataset_description = 'This is the description',
  update_method = 'REPLACE')
```
<br />

## Update A Dataset
`pRoDsUpdate()` function lets you update an existing API dataset from your R environment by leveraging STREAM API that loads data parts at scale.
This function cannot be used to update datasets created from non-API methods.
```r
pRoDsUpdate(client_id = client_id,
  secret = secret,
  dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
  dataset = dataset)
```
<br />

## Query A Dataset
`pRoDsGet()` function lets you query any datasets from your DOMO instance using MySQL.
```r
data <- pRoDsGet(client_id = client_id,
  secret = secret,
  dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
  sql_query = "select * from table")
```
<br />

## Manage Personalized Data Permission (PDP) at Scale
`pRoPdp()` function lets you manage Personalized Data Permission (PDP) at Scale with a master table that contains certain fields such as `Dataset ID`, `Policy Name`, `Policy Column`, `User ID`, and `Policy Value`.
Each combination of `Dataset ID` + `Policy Name` + `Policy Column` will make one policy. 

Here's an example of a master table:
|Dataset ID                           |Policy Name |Policy Column |User ID |Policy Value |
|-------------------------------------|------------|--------------|--------|-------------|
|3c907f1e-846c-4d06-88f6-35592db151f4 |Policy 1    |Column 1      |1234567 |Value 1      |
|3c907f1e-846c-4d06-88f6-35592db151f4 |Policy 1    |Column 1      |1234567 |Value 2      |
|3c907f1e-846c-4d06-88f6-35592db151f4 |Policy 2    |Column 1      |8984038 |Value 1      |
|192ad5cb-c696-4de6-a019-d2cb8dd73e8c |Policy 3    |Column 2      |5427613 |Value A      |
|192ad5cb-c696-4de6-a019-d2cb8dd73e8c |Policy 3    |Column 1      |1234567 |Value A      |
|192ad5cb-c696-4de6-a019-d2cb8dd73e8c |Policy 1    |Column 1      |1234567 |Value A      |

Rows 1 and 2 will be one policy and the rest of the rows will be separate policies, in total there will be 5 policies.

If a combination on the master table doesn't exist on DOMO, the function will create a new policy. 
If a combination on the master table doesn't have the same values on DOMO, the function will update the policy based on what's on the master table.
If an existing combination doesn't exist on the master table, the function will delete the policy. 
```r
pRoPdp(client_id = client_id,
  secret = secret,
  data_table = data_table)
```
<br />

## Manage Group Memberships at Scale
`pRoGroup()` function lets you manage the memberships of groups based on a certain table. The master table must include `group_id` and `user_id` fields. Here's an example:
|group_id     |user_id     |
|-------------|------------|
|324567885    |8984038     |
|324567885    |1234567     |
|324567885    |5427613     |
|637618783    |8984038     |
|637618783    |5427613     |

If a combination of `group_id` and `user_id` on the master table doesn't exist on DOMO, the function will add the user to the group.
If a combination of `group_id` and `user_id` on DOMO doesn't exist on the master table, the function will remove the user from the group. 

```r
pRoGroup(client_id = client_id,
  secret = secret,
  data_table = data_table)
```
<br />

## Retrieve Activity Log
`pRoActivity()` function lets you retrieve all activities within a certain period defined by `start` and `end` parameters. Those parameters can be either POSIX (milliseconds) or date values. 
```r
# Last 24 hours
data <- pRoActivity(client_id = client_id, secret = secret)

# A particular period
data <- pRoActivity(client_id = client_id,
  secret = secret,
  start = as.Date('2024-01-01'),
  end = as.Date('2024-01-02'))

# From a particular date to now
data <- pRoActivity(client_id = client_id,
  secret = secret,
  start = as.Date('2024-01-01'))
```


