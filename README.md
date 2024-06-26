# pRoDomo2
`pRoDomo2` R package provides a seamless integration between DOMO and R programming language. Advanced analytics and a wide range of process automation are possible and made simple by this package.  

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
<br />

## Authentication
This package is based on DOMO's [OAuth authentication](https://developer.domo.com/portal/1845fc11bbe5d-api-authentication), please follow these instructions to obtain `client_id` and `secret`:
1. Get a DOMO instance if you don't already have one: for developers use [developer](https://www.domo.com/start/developer) & students use [student](https://www.domo.com/start/student)
2. Go to [developer.domo.com](https://developer.domo.com/login) and enter the domain of your DOMO instance (the subdomain before ".domo.com")
3. Click "Create a client"
4. Give your client a name and select its scope
5. Click "Create", you'll then be provided with `client_id` and `secret`

<br />
<br />

## Create A Dataset
`pRoDsCreate()` function lets you create an API dataset from your R environment by leveraging STREAM API that loads data parts at scale.
It will then `print()` the dataset_id of the newly created dataset on the console.
```r
pRoDomo2::pRoDsCreate(client_id = client_id,
  secret = secret,
  dataset = dataset,
  dataset_name = 'The Name of the Dataset',
  dataset_description = 'This is the description',
  update_method = 'REPLACE')
```

<br />
<br />

## Update A Dataset
`pRoDsUpdate()` function lets you update an existing API dataset from your R environment by leveraging STREAM API that loads data parts at scale.
This function cannot be used to update datasets created from non-API methods.
```r
pRoDomo2::pRoDsUpdate(client_id = client_id,
  secret = secret,
  dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
  dataset = dataset)
```

<br />
<br />

## Query A Dataset
`pRoDsGet()` function lets you query any datasets from your DOMO instance using MySQL.
```r
data <- pRoDomo2::pRoDsGet(client_id = client_id,
  secret = secret,
  dataset_id = '3c907f1e-846c-4d06-88f6-35592db151f4',
  sql_query = "select * from table")
```

<br />
<br />

## Manage Personalized Data Permission (PDP) at Scale
`pRoPdp()` function lets you manage Personalized Data Permission (PDP) at Scale with a master table that contains certain fields such as `Dataset ID`, `Policy Name`, `Policy Column`, `User ID`, and `Policy Value`.
Each combination of `Dataset ID` + `Policy Name` + `Policy Column` will make one policy. 

Here's an example of a master table:
|Dataset ID                           |Policy Name |Policy Column |User ID |Policy Value |
|:------------------------------------|:-----------|:-------------|:-------|:------------|
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
pRoDomo2::pRoPdp(client_id = client_id,
  secret = secret,
  data_table = data_table)
```

<br />
<br />

## Manage Group Memberships at Scale
`pRoGroup()` function lets you manage the memberships of groups based on a certain table. The master table must include `group_id` and `user_id` fields. Here's an example:
|group_id     |user_id     |
|:------------|:-----------|
|324567885    |8984038     |
|324567885    |1234567     |
|324567885    |5427613     |
|637618783    |8984038     |
|637618783    |5427613     |

If a combination of `group_id` and `user_id` on the master table doesn't exist on DOMO, the function will add the user to the group.
If a combination of `group_id` and `user_id` on DOMO doesn't exist on the master table, the function will remove the user from the group. 

```r
pRoDomo2::pRoGroup(client_id = client_id,
  secret = secret,
  data_table = data_table)
```

<br />
<br />

## Retrieve Activity Log
`pRoActivity()` function lets you retrieve all activities within a certain period defined by `start` and `end` parameters. Those parameters can be either POSIX (milliseconds) or date values. 
```r
# Last 24 hours
data <- pRoDomo2::pRoActivity(client_id = client_id, secret = secret)

# A particular period
data <- pRoDomo2::pRoActivity(client_id = client_id,
  secret = secret,
  start = as.Date('2024-01-01'),
  end = as.Date('2024-01-02'))

# From a particular date to now
data <- pRoDomo2::pRoActivity(client_id = client_id,
  secret = secret,
  start = as.Date('2024-01-01'))
```

<br />
<br />

## Available Functions
1. Activity Log
   * `pRoActivity()` Retrieve Activity Log Entries (Simplified)
   * `activity_log()` Retrieve Activity Log Entries
2. Dataset
   * `pRoDsCreate()` Create A New Dataset Via Stream API
   * `pRoDsGet()` Query a DataSet
   * `pRoDsUpdate()` Update A Dataset Via Stream API
   * `dataset_create()` Create An Empty DataSet
   * `dataset_delete()` Delete a DataSet
   * `dataset_export()` Export data from a DOMO DataSet
   * `dataset_get_info_all()` List DataSets
   * `dataset_get_info()` Retrieve a DataSet
   * `dataset_import()` Import data into DataSet
   * `dataset_query()` Query a DataSet
   * `dataset_update_info()` Update a DataSet
3. Group
   * `pRoGroup()` Group Management
   * `group_create()` Create a group
   * `group_delete()` Delete a group
   * `group_get_all()` List groups
   * `group_get()` Retrieve a group
   * `group_update()` Update a group
   * `group_user_add()` Add a user to a group
   * `group_user_get()` List users in a group
   * `group_user_remove()` Remove a user from a group
4. Page
   * `pRoPageGet()` Retrieve All Pages
   * `page_collection_create()` Create a page collection
   * `page_collection_delete()` Delete a page collection
   * `page_collection_get()` Retrieve a page collection
   * `page_collection_update()` Update a page collection
   * `page_create()` Create a page
   * `page_delete()` Delete a page
   * `page_get_all()` List pages
   * `page_get()` Retrieve a page
   * `page_update()` Update a page
5. Personalized Data Permission (PDP)
   * `pRoPdp()` Manage Personalized Data Permission (PDP) at Scale
   * `pRoPdpGet()` List Personalized Data Permission (PDP) policies
   * `pdp_create()` Create a Personalized Data Permission (PDP) Policy
   * `pdp_delete()` Delete a Personalized Data Permission (PDP) Policy
   * `pdp_get_all()` List Personalized Data Permission (PDP) policies
   * `pdp_get()` Retrieve a Personalized Data Permission (PDP) policy
   * `pdp_update()` Update a Personalized Data Permission (PDP) policy
6. Project, List, & Task
   * `plt_attachment_add()` Add attachment
   * `plt_attachment_delete()` Delete a project
   * `plt_attachment_download()` Download attachment
   * `plt_attachment_get()` Retrieve a list of attachments
   * `plt_list_create()` Create a list
   * `plt_list_delete()` Delete a list
   * `plt_list_get_all()` Retrieve all project lists
   * `plt_list_get()` Retrieve Individual List
   * `plt_list_update()` Update a list
   * `plt_project_create()` Create a project
   * `plt_project_delete()` Delete a project
   * `plt_project_get_all()` Retrieve all projects
   * `plt_project_get()` Retrieve individual project
   * `plt_project_member_get()` Retrieve project members
   * `plt_project_member_update()` Update project members
   * `plt_project_update()` Update individual project
   * `plt_task_create()` Create a task
   * `plt_task_get_all()` Retrieve all project tasks
   * `plt_task_get()` Retrieve individual task
   * `plt_task_update()` Update a Task
7. Stream
   * `stream_create()` Create a Stream
   * `stream_delete()` Delete a Stream
   * `stream_execution_abort()` Abort a Stream execution
   * `stream_execution_commit()` Commit a Stream execution
   * `stream_execution_create()` Create a Stream execution
   * `stream_execution_get_all()` List Stream executions
   * `stream_execution_get()` Retrieve a Stream execution
   * `stream_get_all()` List Streams
   * `stream_get()` Retrieve a Stream
   * `stream_search()` Search Streams
   * `stream_update()` Update a Stream
   * `stream_uploadPart()` Upload a data part
8. User
   * `user_create()` Create A User
   * `user_delete()` Delete A User
   * `user_get_all()` List users
   * `user_get()` Retrieve a user
   * `user_update()` Update A User

