#' Get an AppDB Collection's Permissions
#'
#' Lists who holds which permissions on one or more AppDB collections, through
#' \code{GET /api/datastores/v1/collections/\{id\}/permission}.
#'
#' @param collection_id One or more AppDB collection ids (GUIDs, not names; see
#'   \code{appdb_collection_get_all()}).
#' @param developer_token A valid Domo Developer Token.
#' @param instance Domo instance URL. For example:
#'   \code{"https://company.domo.com"}. A bare hostname is also accepted;
#'   \code{https://} is added automatically when no scheme is present.
#'
#' @return A tibble with one row per collection and entity: \code{collection_id},
#'   \code{entity} (\code{"USER"}, \code{"GROUP"} or \code{"RYUU_APP"}),
#'   \code{entity_id} and \code{permissions} (lower case, sorted, comma
#'   separated). A collection nobody holds a permission on gives no rows.
#'
#' @examples
#' \dontrun{
#' appdb_permission_get("12345678-1234-1234-1234-123456789012",
#'                      Sys.getenv("DOMO_TOKEN"), "https://company.domo.com")
#' }
#'
#' @export

appdb_permission_get <- function(collection_id, developer_token, instance) {
  ids <- .appdb_perm_check_collections(collection_id)
  .dataflow_check_token(developer_token)
  rows <- lapply(ids, function(id) {
    .appdb_perm_read(id, developer_token, instance)
  })
  dplyr::bind_rows(rows)
}

#' Add Permissions to AppDB Collections
#'
#' Adds permissions for users, groups or app instances to AppDB collections,
#' keeping whatever each one already holds (\code{overwrite=false}). Several
#' collections and several entities can be given at once; every combination
#' gets the same permissions.
#'
#' @inheritParams appdb_permission_get
#' @param entity The kind of entity: \code{"USER"}, \code{"GROUP"} or
#'   \code{"RYUU_APP"}. One value, or one per \code{entity_id}.
#' @param entity_id The numeric id of each user or group (or the instance UUID
#'   of an app).
#' @param permissions One or more of \code{admin}, \code{write}, \code{read},
#'   \code{share}, \code{delete}, \code{create_content}, \code{update_content},
#'   \code{read_content}, \code{delete_content}.
#' @param dry_run When \code{TRUE} (the default) nothing is changed: the current
#'   permissions are read and the result shows what would happen. Set
#'   \code{FALSE} to apply the change.
#'
#' @details
#' Domo always stores \code{read} together with any other permission, so
#' \code{before} and \code{after} include it even when it was not asked for.
#'
#' A real run changes only what is not already in place, then reads every
#' collection back and stops with an error if a change did not land. Users and
#' groups inherit the app's permissions, so a direct grant is only needed when
#' someone should have less (or more) than the app gives.
#'
#' @return A tibble, one row per collection and entity: \code{collection_id},
#'   \code{entity}, \code{entity_id}, \code{action}, \code{before},
#'   \code{after}, \code{status} (\code{"would change"}, \code{"no change"},
#'   \code{"applied"} or \code{"already in place"}) and, on a real run,
#'   \code{verified}.
#'
#' @examples
#' \dontrun{
#' appdb_permission_add(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   entity = "GROUP", entity_id = "870010733",
#'   permissions = c("read", "read_content", "create_content"),
#'   developer_token = Sys.getenv("DOMO_TOKEN"),
#'   instance = "https://company.domo.com"
#' )                                   # dry run: shows the plan only
#' }
#'
#' @export

appdb_permission_add <- function(
  collection_id,
  entity,
  entity_id,
  permissions,
  developer_token,
  instance,
  dry_run = TRUE
) {
  changes <- .appdb_perm_grid(collection_id, entity, entity_id, permissions)
  .appdb_perm_apply(changes, "add", developer_token, instance, dry_run)
}

#' Replace Permissions on AppDB Collections
#'
#' Sets the permissions of users, groups or app instances on AppDB collections
#' to exactly the ones given (\code{overwrite=true}), discarding what each held
#' before. Use \code{appdb_permission_add()} to keep existing permissions.
#'
#' @inheritParams appdb_permission_add
#'
#' @inherit appdb_permission_add details return
#'
#' @examples
#' \dontrun{
#' appdb_permission_update(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   entity = "USER", entity_id = "540824222",
#'   permissions = c("read", "read_content"),
#'   developer_token = Sys.getenv("DOMO_TOKEN"),
#'   instance = "https://company.domo.com",
#'   dry_run = FALSE
#' )
#' }
#'
#' @export

appdb_permission_update <- function(
  collection_id,
  entity,
  entity_id,
  permissions,
  developer_token,
  instance,
  dry_run = TRUE
) {
  changes <- .appdb_perm_grid(collection_id, entity, entity_id, permissions)
  .appdb_perm_apply(changes, "update", developer_token, instance, dry_run)
}

#' Remove All Permissions of an Entity from AppDB Collections
#'
#' Removes every permission a user, group or app instance holds on the given
#' AppDB collections. If that leaves a collection with no entities at all, only
#' users with the Manage AppDB grant can still reach it.
#'
#' @inheritParams appdb_permission_add
#'
#' @inherit appdb_permission_add details return
#'
#' @examples
#' \dontrun{
#' appdb_permission_delete(
#'   collection_id = "12345678-1234-1234-1234-123456789012",
#'   entity = "GROUP", entity_id = "870010733",
#'   developer_token = Sys.getenv("DOMO_TOKEN"),
#'   instance = "https://company.domo.com"
#' )
#' }
#'
#' @export

appdb_permission_delete <- function(
  collection_id,
  entity,
  entity_id,
  developer_token,
  instance,
  dry_run = TRUE
) {
  changes <- .appdb_perm_grid(collection_id, entity, entity_id, NULL)
  .appdb_perm_apply(changes, "delete", developer_token, instance, dry_run)
}

#' Bulk Change AppDB Collection Permissions
#'
#' Applies a table of permission changes in one go, for when different
#' collections, entities or permission sets are involved. Same rules, dry run
#' and read-back as \code{appdb_permission_add()}.
#'
#' @inheritParams appdb_permission_add
#' @param changes A data frame with one row per change and columns
#'   \code{collection_id}, \code{entity}, \code{entity_id} and (not needed for
#'   \code{action = "delete"}) \code{permissions}, a comma separated string such
#'   as \code{"read,read_content"}.
#' @param action \code{"add"} (keep what is held), \code{"update"} (replace) or
#'   \code{"delete"} (remove all).
#'
#' @inherit appdb_permission_add details return
#'
#' @examples
#' \dontrun{
#' changes <- data.frame(
#'   collection_id = c("12345678-1234-1234-1234-123456789012",
#'                     "22345678-1234-1234-1234-123456789012"),
#'   entity = "GROUP",
#'   entity_id = "870010733",
#'   permissions = c("read,read_content", "read,read_content,create_content")
#' )
#' appdb_permission_bulk(changes, "add", Sys.getenv("DOMO_TOKEN"),
#'                       "https://company.domo.com")
#' }
#'
#' @export

appdb_permission_bulk <- function(
  changes,
  action = c("add", "update", "delete"),
  developer_token,
  instance,
  dry_run = TRUE
) {
  action <- match.arg(action)
  needed <- c("collection_id", "entity", "entity_id",
              if (action != "delete") "permissions")
  if (!is.data.frame(changes) || nrow(changes) == 0 ||
      !all(needed %in% names(changes))) {
    stop("`changes` must be a data frame with rows and the columns: ",
         paste(needed, collapse = ", "), ".", call. = FALSE)
  }
  changes <- data.frame(
    collection_id = as.character(changes$collection_id),
    entity = as.character(changes$entity),
    entity_id = as.character(changes$entity_id),
    permissions = if (action == "delete") NA_character_
                  else as.character(changes$permissions),
    stringsAsFactors = FALSE
  )
  .appdb_perm_validate(changes, action)
  .appdb_perm_apply(changes, action, developer_token, instance, dry_run,
                    validated = TRUE)
}

# Internal helpers. Not exported (leading ".").

.appdb_perm_entities <- c("USER", "GROUP", "RYUU_APP")

.appdb_perm_allowed <- c(
  "admin", "write", "read", "share", "delete",
  "create_content", "update_content", "read_content", "delete_content"
)

.appdb_perm_check_collections <- function(x) {
  ok <- is.character(x) && length(x) > 0 && !anyNA(x) &&
    all(grepl("^[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}$", x))
  if (!ok) {
    stop("`collection_id` must be one or more collection ids (GUIDs, not names).",
         call. = FALSE)
  }
  tolower(unique(x))
}

# "read_content, READ" -> c("read", "read_content"): lower case, sorted, unique.
.appdb_perm_split <- function(x) {
  parts <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  sort(unique(tolower(parts[nzchar(parts) & !is.na(parts)])))
}

# Every collection x entity_id combination, one permission set for all.
.appdb_perm_grid <- function(collection_id, entity, entity_id, permissions) {
  collections <- .appdb_perm_check_collections(collection_id)
  if (!is.character(entity_id) || length(entity_id) == 0 || anyNA(entity_id) ||
      any(!grepl("^[0-9A-Za-z-]+$", entity_id))) {
    stop("`entity_id` must be one or more ids as character strings.",
         call. = FALSE)
  }
  entity <- toupper(as.character(entity))
  if (length(entity) == 1) entity <- rep(entity, length(entity_id))
  if (length(entity) != length(entity_id)) {
    stop("`entity` must be one value, or one per `entity_id`.", call. = FALSE)
  }
  perms <- if (is.null(permissions)) NA_character_
           else paste(.appdb_perm_split(permissions), collapse = ",")
  who <- data.frame(entity = entity, entity_id = entity_id,
                    stringsAsFactors = FALSE)
  who <- who[!duplicated(who), , drop = FALSE]
  out <- do.call(rbind, lapply(collections, function(cid) {
    data.frame(collection_id = cid, who, permissions = perms,
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

.appdb_perm_validate <- function(changes, action) {
  if (anyNA(changes$collection_id) || anyNA(changes$entity_id)) {
    stop("`collection_id` and `entity_id` cannot be missing.", call. = FALSE)
  }
  .appdb_perm_check_collections(changes$collection_id)
  changes$entity <- toupper(changes$entity)
  if (!all(changes$entity %in% .appdb_perm_entities)) {
    stop("`entity` must be one of: ",
         paste(.appdb_perm_entities, collapse = ", "), ".", call. = FALSE)
  }
  if (any(!grepl("^[0-9A-Za-z-]+$", changes$entity_id))) {
    stop("`entity_id` must be ids as character strings.", call. = FALSE)
  }
  if (action != "delete") {
    sets <- lapply(changes$permissions, .appdb_perm_split)
    bad <- unique(unlist(lapply(sets, setdiff, .appdb_perm_allowed)))
    if (any(lengths(sets) == 0) || length(bad) > 0) {
      stop("`permissions` must name at least one of: ",
           paste(.appdb_perm_allowed, collapse = ", "), ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# GET one collection's permission table as a tibble (empty when none).
.appdb_perm_read <- function(collection_id, developer_token, instance) {
  url <- paste0(.normalize_domo_instance(instance),
                "/api/datastores/v1/collections/", collection_id, "/permission")
  body <- httr2::resp_body_json(.appdb_perm_perform(url, developer_token))
  rows <- lapply(names(body), function(kind) {
    lapply(body[[kind]], function(e) {
      list(collection_id = collection_id, entity = kind,
           entity_id = as.character(e$id),
           permissions = paste(sort(tolower(unlist(e$permissions))),
                               collapse = ","))
    })
  })
  rows <- unlist(rows, recursive = FALSE)
  if (length(rows) == 0) {
    return(dplyr::tibble(collection_id = character(), entity = character(),
                         entity_id = character(), permissions = character()))
  }
  dplyr::bind_rows(rows)
}

# One request. PUT and DELETE are idempotent, so retrying on 429/503 is safe.
# Only Domo's own error message is attached to a failure, so the token cannot
# leak.
.appdb_perm_perform <- function(url, developer_token, method = "GET",
                                query = list()) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package \"httr2\" must be installed to use this function.",
         call. = FALSE)
  }
  request <- httr2::request(url)
  request <- httr2::req_method(request, method)
  if (length(query) > 0) request <- httr2::req_url_query(request, !!!query)
  request <- httr2::req_headers(request,
                                `X-DOMO-Developer-Token` = developer_token,
                                Accept = "application/json")
  request <- httr2::req_retry(
    request,
    max_tries = 3,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
  )
  request <- .appdb_req_error(request)
  httr2::req_perform(request)
}

# Domo stores `read` with every permission it is given (granting only
# `create_content` is stored as `create_content,read`), so the planned and
# read-back sets include it.

# Plan every change against the current permissions, and (unless dry_run) apply
# the ones that are not already in place and read everything back.
.appdb_perm_apply <- function(changes, action, developer_token, instance,
                              dry_run, validated = FALSE) {
  .dataflow_check_token(developer_token)
  if (!is.logical(dry_run) || length(dry_run) != 1 || is.na(dry_run)) {
    stop("`dry_run` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!validated) .appdb_perm_validate(changes, action)
  changes$entity <- toupper(changes$entity)
  base <- .normalize_domo_instance(instance)

  current <- dplyr::bind_rows(lapply(unique(changes$collection_id), function(id) {
    .appdb_perm_read(id, developer_token, instance)
  }))

  held <- function(cid, kind, eid) {
    hit <- current$permissions[current$collection_id == cid &
                                 current$entity == kind &
                                 current$entity_id == eid]
    if (length(hit) == 0) character(0) else .appdb_perm_split(hit[[1]])
  }

  plan <- do.call(rbind, lapply(seq_len(nrow(changes)), function(i) {
    ch <- changes[i, ]
    before <- held(ch$collection_id, ch$entity, ch$entity_id)
    wanted <- if (action == "delete") character(0) else .appdb_perm_split(ch$permissions)
    # Domo always stores `read` alongside any other permission.
    after <- switch(action,
      add = sort(union(union(before, wanted), "read")),
      update = sort(union(wanted, "read")),
      delete = character(0)
    )
    data.frame(
      collection_id = ch$collection_id, entity = ch$entity,
      entity_id = ch$entity_id, action = action,
      before = paste(before, collapse = ","),
      after = paste(after, collapse = ","),
      wanted = paste(wanted, collapse = ","),
      change = !identical(before, after),
      stringsAsFactors = FALSE
    )
  }))

  if (dry_run) {
    plan$status <- ifelse(plan$change, "would change", "no change")
    return(dplyr::as_tibble(plan[, c("collection_id", "entity", "entity_id",
                                     "action", "before", "after", "status")]))
  }

  for (i in which(plan$change)) {
    p <- plan[i, ]
    url <- paste0(base, "/api/datastores/v1/collections/", p$collection_id,
                  "/permission/", p$entity, "/", p$entity_id)
    if (action == "delete") {
      .appdb_perm_perform(url, developer_token, "DELETE")
    } else {
      .appdb_perm_perform(
        url, developer_token, "PUT",
        query = list(
          permissions = paste(.appdb_perm_split(p$wanted), collapse = ","),
          overwrite = if (action == "update") "true" else "false"
        )
      )
    }
  }

  after <- dplyr::bind_rows(lapply(unique(plan$collection_id), function(id) {
    .appdb_perm_read(id, developer_token, instance)
  }))
  plan$verified <- vapply(seq_len(nrow(plan)), function(i) {
    hit <- after$permissions[after$collection_id == plan$collection_id[i] &
                               after$entity == plan$entity[i] &
                               after$entity_id == plan$entity_id[i]]
    now <- if (length(hit) == 0) character(0) else .appdb_perm_split(hit[[1]])
    identical(now, .appdb_perm_split(plan$after[i]))
  }, logical(1))
  plan$status <- ifelse(plan$change, "applied", "already in place")

  if (!all(plan$verified)) {
    stop(sum(!plan$verified), " of ", nrow(plan),
         " change(s) did not show up when read back. Check the collections' ",
         "permissions in Domo before trying again.", call. = FALSE)
  }
  dplyr::as_tibble(plan[, c("collection_id", "entity", "entity_id", "action",
                            "before", "after", "status", "verified")])
}
