# CaRM

`CaRM` is a light CRM system. It:

* keeps track of contact requests from customers
* provides spam protection
* sends email notifications
* runs a webserver to expose data to internal users (with authentication)

# Installation

Download the source, then `make` and `./carm/carm`. It will start on port 4200.

The `carm` folder contains the entire `CaRM` installation, including the database and config file.

# Internals

`CaRM` runs a Hunchentoot server that serves the JSON API and the static frontend assets. 

The backend is written in Common Lisp. It persists data in a SQLite database and mostly serves to read
and write to it. It also sends email notifications and will be used for automating tasks.

The frontend is plain HTML+JS. It renders data received from the API. It is usable on mobile.

# Config

`carm.conf` is the config file. It is a JSON object:

	{
		port: 4200,
		required-fields: ["telephone", "email"],
		forbidden-fields: ["fakebody"]
	}

# API

## Authentication

	POST /carm/v1/api/auth
	Authorization: Basic user:pass
	Returns:
	{ "session-id": string }
	
Where `user:pass` is base64 encoded. The `session-id` is provided in the `Authorization` header of the
following authorized requests (as-is). 

All the following requests are assumed to be authorized unless stated otherwise. In case of an authorization
failure, `403 FORBIDDEN` is thrown.

## Contact Requests

Create a new contact request:

	POST /carm/v1/api/contact-request
	Accepts: a string map (i.e. any flat json object containing only strings)
	Example: {
		name: "foo",
		message: "bar"
	}
	Returns: 204 NO CONTENT or 400 BAD REQUEST
	
`400` will only be thrown if one of the required fields is not set, but not if one of the forbidden fields is
set (in which case `204` would be returned).

Get contact requests:

	GET /carm/v1/api/contact-request
	Returns: array of contact requests
	Example: [ 
		{id: 1, name:"foo", message:"bar", timestamp: 1608770775}, 
		{id: 2, message: "foobar", timestamp: 1608770775} 
	]
	
Contact requests are string maps except for `id` and `timestamp`, which are `number`s. `id`s are guaranteed
to be unique and not necessarily sequential.

Get specific contact request:

	GET /carm/v1/api/contact-request/:id
	Returns: contact request
	Example: {id: 1, name:"foo", message:"bar", timestamp: 1608770775}

### Required and forbidden fields

Required fields are necessary to store a request. If they are not found in the `POST` body, the request will `400`.

Forbidden fields are used to honeypot spammers. In the frontend, one may include them in the form but hide them,
making it so only the bots complete them. If one of the forbidden field is found, the contact request returns `204`,
but discards all data.

