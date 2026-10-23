cid <- "cbffcce3-f50a-4b7e-a41d-7042e36df54b"
cid2 <- "f19a2283-f353-4fcf-a7ac-7be39a1407c0"

token_of <- function(req) {
  httr2::req_get_headers(req, redacted = "reveal")[["X-DOMO-Developer-Token"]]
}

# A fake Domo: `store` maps collection id -> list(KIND = list(id -> perms)).
# GET returns the table, PUT/DELETE change it, and every request is recorded.
fake_domo <- function(store, ignore_writes = FALSE, frame = parent.frame()) {
  env <- new.env()
  env$store <- store
  env$calls <- list()
  httr2::local_mocked_responses(function(req) {
    env$calls[[length(env$calls) + 1]] <- req
    method <- httr2::req_get_method(req)
    parts <- strsplit(sub("^https://x.domo.com/api/datastores/v1/collections/", "",
                          sub("\\?.*$", "", req$url)), "/")[[1]]
    id <- parts[1]
    if (method == "GET") {
      table <- lapply(env$store[[id]], function(kind) {
        lapply(names(kind), function(eid) {
          list(id = eid, permissions = as.list(toupper(kind[[eid]])))
        })
      })
      return(httr2::response_json(body = table))
    }
    if (!ignore_writes) {
      kind <- parts[3]
      eid <- parts[4]
      if (method == "DELETE") {
        env$store[[id]][[kind]][[eid]] <- NULL
      } else {
        query <- httr2::url_parse(req$url)$query
        wanted <- union(strsplit(query$permissions, ",")[[1]], "read")  # Domo always adds read
        old <- env$store[[id]][[kind]][[eid]]
        env$store[[id]][[kind]][[eid]] <-
          if (identical(query$overwrite, "true")) wanted else union(old, wanted)
      }
    }
    httr2::response(status_code = 200)
  }, env = frame)
  env
}

base_store <- function() {
  list(
    GROUP = list("111" = c("read", "read_content")),
    USER = list("222" = c("admin"))
  )
}

writes <- function(env) {
  Filter(function(r) httr2::req_get_method(r) != "GET", env$calls)
}

test_that("inputs are checked before any request", {
  expect_error(appdb_permission_get("not-a-guid", "tok", "x.domo.com"), "collection_id")
  expect_error(appdb_permission_get(cid, "", "x.domo.com"), "developer_token")
  expect_error(appdb_permission_add(cid, "TEAM", "111", "read", "tok", "x.domo.com"), "entity")
  expect_error(appdb_permission_add(cid, "GROUP", "111", "reed", "tok", "x.domo.com"), "permissions")
  expect_error(appdb_permission_add(cid, "GROUP", "111", character(0), "tok", "x.domo.com"), "permissions")
  expect_error(appdb_permission_add(cid, "GROUP", NA_character_, "read", "tok", "x.domo.com"), "entity_id")
  expect_error(appdb_permission_add(cid, "GROUP", "111", "read", "tok", "x.domo.com", dry_run = NA), "dry_run")
  expect_error(appdb_permission_bulk(data.frame(collection_id = cid), "add", "tok", "x.domo.com"), "changes")
})

test_that("appdb_permission_get returns one row per entity", {
  env <- fake_domo(setNames(list(base_store()), cid))
  res <- appdb_permission_get(cid, "secret-token", "x.domo.com")
  expect_identical(env$calls[[1]]$url,
                   paste0("https://x.domo.com/api/datastores/v1/collections/", cid, "/permission"))
  expect_identical(token_of(env$calls[[1]]), "secret-token")
  expect_equal(nrow(res), 2)
  expect_setequal(res$entity, c("GROUP", "USER"))
  expect_identical(res$permissions[res$entity == "GROUP"], "read,read_content")
})

test_that("a collection with no permissions gives an empty table", {
  fake_domo(setNames(list(list()), cid))
  res <- appdb_permission_get(cid, "tok", "x.domo.com")
  expect_equal(nrow(res), 0)
  expect_named(res, c("collection_id", "entity", "entity_id", "permissions"))
})

test_that("a dry run reads but never writes", {
  env <- fake_domo(setNames(list(base_store()), cid))
  res <- appdb_permission_add(cid, "GROUP", "111", c("create_content", "read"), "tok", "x.domo.com")
  expect_length(writes(env), 0)
  expect_identical(res$status, "would change")
  expect_identical(res$before, "read,read_content")
  expect_identical(res$after, "create_content,read,read_content")
  expect_false("verified" %in% names(res))
})

test_that("add keeps existing permissions and reads the result back", {
  env <- fake_domo(setNames(list(base_store()), cid))
  res <- appdb_permission_add(cid, "GROUP", "111", "create_content", "tok", "x.domo.com", dry_run = FALSE)
  w <- writes(env)
  expect_length(w, 1)
  expect_identical(httr2::req_get_method(w[[1]]), "PUT")
  expect_match(w[[1]]$url, paste0(cid, "/permission/GROUP/111"), fixed = TRUE)
  query <- httr2::url_parse(w[[1]]$url)$query
  expect_identical(query$permissions, "create_content")
  expect_identical(query$overwrite, "false")
  expect_identical(res$status, "applied")
  expect_true(res$verified)
  expect_identical(res$after, "create_content,read,read_content")
})

test_that("adding what is already held sends nothing", {
  env <- fake_domo(setNames(list(base_store()), cid))
  res <- appdb_permission_add(cid, "GROUP", "111", "read", "tok", "x.domo.com", dry_run = FALSE)
  expect_length(writes(env), 0)
  expect_identical(res$status, "already in place")
  expect_true(res$verified)
})

test_that("update replaces the set with overwrite=true", {
  env <- fake_domo(setNames(list(base_store()), cid))
  res <- appdb_permission_update(cid, "GROUP", "111", "read", "tok", "x.domo.com", dry_run = FALSE)
  w <- writes(env)
  expect_length(w, 1)
  expect_identical(httr2::url_parse(w[[1]]$url)$query$overwrite, "true")
  expect_identical(res$after, "read")
  expect_true(res$verified)
})

test_that("delete removes the entity", {
  env <- fake_domo(setNames(list(base_store()), cid))
  res <- appdb_permission_delete(cid, "USER", "222", "tok", "x.domo.com", dry_run = FALSE)
  w <- writes(env)
  expect_length(w, 1)
  expect_identical(httr2::req_get_method(w[[1]]), "DELETE")
  expect_identical(res$after, "")
  expect_true(res$verified)
  # deleting something that is not there changes nothing
  env2 <- fake_domo(setNames(list(base_store()), cid))
  res2 <- appdb_permission_delete(cid, "USER", "999", "tok", "x.domo.com", dry_run = FALSE)
  expect_length(writes(env2), 0)
  expect_identical(res2$status, "already in place")
})

test_that("several collections and entities are all handled", {
  store <- list(base_store(), list(USER = list("222" = "admin")))
  names(store) <- c(cid, cid2)
  env <- fake_domo(store)
  res <- appdb_permission_add(c(cid, cid2), "GROUP", c("111", "333"), "read",
                              "tok", "x.domo.com", dry_run = FALSE)
  expect_equal(nrow(res), 4)
  # 111 already has read on the first collection; the other three are new
  expect_equal(sum(res$status == "applied"), 3)
  expect_length(writes(env), 3)
  expect_true(all(res$verified))
  gets <- Filter(function(r) httr2::req_get_method(r) == "GET", env$calls)
  expect_length(gets, 4)  # one read per collection before, one after
})

test_that("bulk takes a table with a different permission set per row", {
  env <- fake_domo(setNames(list(base_store()), cid))
  changes <- data.frame(
    collection_id = cid, entity = c("GROUP", "USER"), entity_id = c("111", "333"),
    permissions = c("create_content", "read, read_content"),
    stringsAsFactors = FALSE
  )
  res <- appdb_permission_bulk(changes, "add", "tok", "x.domo.com", dry_run = FALSE)
  expect_length(writes(env), 2)
  expect_identical(res$after, c("create_content,read,read_content", "read,read_content"))
  expect_true(all(res$verified))
})

test_that("a change that does not show up when read back is an error", {
  fake_domo(setNames(list(base_store()), cid), ignore_writes = TRUE)
  expect_error(
    appdb_permission_add(cid, "GROUP", "111", "create_content", "tok", "x.domo.com", dry_run = FALSE),
    "did not show up"
  )
})

test_that("the token never appears in an error", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 404, body = list(message = "Collection not found"))
  })
  err <- tryCatch(appdb_permission_get(cid, "secret-token", "x.domo.com"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "Collection not found")
  expect_false(grepl("secret-token", err, fixed = TRUE))
})

test_that("read is always part of the planned and stored set", {
  env <- fake_domo(setNames(list(base_store()), cid))
  # a new entity asked for one non-read permission: the plan already shows read
  dry <- appdb_permission_update(cid, "GROUP", "333", "create_content", "tok", "x.domo.com")
  expect_identical(dry$after, "create_content,read")
  res <- appdb_permission_update(cid, "GROUP", "333", "create_content", "tok", "x.domo.com", dry_run = FALSE)
  expect_true(res$verified)
  expect_identical(res$after, "create_content,read")
  # asking again for the same thing is a no-op, not a repeat write
  before <- length(writes(env))
  again <- appdb_permission_update(cid, "GROUP", "333", "create_content", "tok", "x.domo.com", dry_run = FALSE)
  expect_identical(again$status, "already in place")
  expect_length(writes(env), before)
})
