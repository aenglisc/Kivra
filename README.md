# Kivra Backend candidate case

The backend should be composed of 2 APIs:
  * Sender API: upload a file in binary format with metadata
  * Consumer API: query by receiver id, and pay for content
  
## Requirements
  * `make`
  * `docker`

## Instructions
  target|description
  -|-
  `make`|Setup the dev environment
  `make setup`|Setup the dev environment
  `make up`|Start the dev environment
  `make down`|Stop the dev environment
  `make generate`|Generate API stubs
  `make generate_{sender,consumer}`|Generate API stubs for a particular api
  `make interactive_start_{sender,consumer}`|Run an api with a shell
  `make test`|Run tests
  `make test_{sender,consumer}`|Run tests for a paritcular api
  `make shell_{sender,consumer}`|Run a terminal session inside one of the containers
  `make pg_shell`|Run a psql session inside the postgres container
