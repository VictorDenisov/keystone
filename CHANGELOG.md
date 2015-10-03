# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [0.1.0.0] - 2015-09-17

### Added

- Versions
  * List versions - /
  * Show v3 version details - /v3

- Tokens
  * Authenticate using password method - POST /v3/auth/tokens
  * Validate token - GET /v3/auth/tokens
  * Check token - HEAD /v3/auth/tokens

- Services
  * Create service - POST /v3/services
  * List all services - POST /v3/services
  * Get service details - GET /v3/services/:id
  * Update service - PATCH /v3/services/:id
  * Delete service - DELETE /v3/services/:id

- Endpoints
  * Create endpoint - POST /v3/endpoints
  * List all endpoints - GET /v3/endpoints
  * Get endpoint details - GET /v3/endpoints/:id
  * Delete endpoint - DELETE /v3/endpoints/:id

- Domains
  * List all domains - POST /v3/domains
  * Get domain details - GET /v3/domains/:id

- Projects
  * Create project - POST /v3/projects
  * List projects - GET /v3/projects
  * Get project details - GET /v3/projects/:id
  * Delete project - DELETE /v3/projects/:id
  * List roles for project user - GET /v3/projects/:pid/users/:uid/roles
  * Add role to a project user - GET /v3/projects/:pid/users/:uid/roles/:rid

- Users
  * Create user - POST /v3/users
  * List all users - GET /v3/users
  * Get user details - GET /v3/users/:id
  * Update user - PATCH /v3/users/:id
  * Delete user - DELETE /v3/users/:id
  * List all projects this user is assigned to - GET /v3/users/:uid/projects
  * Update password - POST /v3/users/:uid/password

- Roles
  * Create role - POST /v3/roles
  * List all roles - GET /v3/roles
  * Get role details - GET /v3/roles/:id
  * Delete role - DELETE /v3/roles/:id
  * List role assignments - GET /v3/role_assignments

- Config file
  * admin token
  * certificate for ssl server
  * keyfile for ssl server
  * port for listening
  * default endpoint to be returned
  * database
    + database host
    + database port
  * log level
  * server type (TLS - https endpoint, Plain - http endpoint)
  * flag if you want keystone to verify token collection befor startup

- Policy file
