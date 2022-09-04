# Kivra Backend candidate case

The backend should be composed of 2 APIs:
  * Sender API: accept a file in binary format with metadata:
    * sender id
    * file type
    * receiver id
    * payable?
  * Receiver API: query by receiver id, pay
  
## Requirements
  * `make`
  * `docker`

## Instructions
  target|description
  -|-
  `make up`|Start the dev environment
  `make down`|Stop the dev environment

