# CaRM

`CaRM` is a light CRM system. It:

* keeps track of contact requests from customers
* provides spam protection
* synchronizes Google Sheets spreadsheets with the contact request data
* sends email notifications
* runs a webserver to expose data to internal users (with authentication)

# Installation

Download the source, then `make` `cd carm` and `./carm carm.conf`. It will start on port 4200.

The `carm` folder contains the entire `CaRM` installation, including the database and config file.
It also contains the static frontend assets.

# Internals

`CaRM` runs a Hunchentoot server that serves the JSON API and the static frontend assets. 

The backend is written in Common Lisp. It persists data in a SQLite database and mostly serves to read
and write to it. It also sends email notifications and will be used for automating tasks.

The frontend is plain HTML+JS. It renders data received from the API. It is usable on mobile.

# Config

`carm.conf` is the config file. It is a JSON object:

	{
		port: 4200,
		contactRequiredFields: ["telephone", "email"],
		contactForbiddenFields: ["fakebody"],
		mailType: "smtp",
		smptServer: "smpt.example.com",
		smtpUser: "foobar",
		smtpPass: "hunter2",
		googleIamServiceEmail: "iam.example@example.com",
		googleRsaKeyPath: "./keyrsa.pem",
		contactRequestSpreadsheetId: "#spreadsheetId here#",
		contactRequestSheetNames: {"example.com": "Sheet1" },
		contactRequestNotificationEmail: "example@example.com",
		contactRequestSheetId: 0
	}

# API

## Authentication

Basic:

	POST /carm/v1/api/auth
	Authorization: Basic user:pass
	Returns: 200 OK or 403 FORBIDDEN
	{ sessionId: string }
	
Where `user:pass` is base64 encoded. The `session-id` is returned base64 encoded and can be used for `Basic`
authentication as-is. 

All the following requests are assumed to be authorized unless stated otherwise. In case of an authorization
failure, `403 FORBIDDEN` is thrown.

To invalidate a session:

	DELETE /carm/v1/api/auth
	Returns: 204 NO CONTENT
	
Session are invalidated after 48 hours normally.

## Users

Get current user:

	GET /carm/v1/api/user
	Returns: 200 OK, user object
	Example: {
		username: "foo",
		email: "bar@example.com",
		createdAt: 3818107279
	}

## Contact Requests

Create a new contact request (does not require authentication):

	POST /carm/v1/api/contact-request
	Accepts: a string map (i.e. any flat json object containing only strings)
	Example: {
		name: "foo",
		message: "bar"
	}
	Returns: 204 NO CONTENT or 400 BAD REQUEST
	
`400` will only be thrown if one of the required fields is not set, but not if one of the forbidden fields is
set (in which case `204` would be returned and the data discarded).

Get contact requests:

	GET /carm/v1/api/contact-request?offset=<integer>&limit=<integer>
	Returns: 200 OK, objects containing array of contact requests
	Example: {
		contactRequests: [ 
		{id: 1, name:"foo", message:"bar", seen: true, timestamp: 3818107279}, 
		{id: 2, message: "foobar", seen: false, timestamp: 3818107279} 
		]
	}

`offset` and `limit` are used for pagination and optional.
	
Contact requests are objects which contain the original string map, in addition to:

* `id` and `timestamp`: numbers. `id`s are guaranteed to be unique and not necessarily sequential.
* `host`: the TLD of the website initiating the request. Used to know how to process request (by spreadsheet and email)
* `seen`: boolean. Used in frontend.

To set the `seen` field in contact requests to true/false:

	PUT /carm/v1/api/contact-request/:id/seen
	Returns: 204 NO CONTENT
	      or 404 NOT FOUND
	DELETE /carm/v1/api/contact-request/:id/seen
	

Get specific contact request:

	GET /carm/v1/api/contact-request/:id
	Returns: 200 OK, contact request
	      or 404 NOT FOUND
	Example: {id: 1, name:"foo", message:"bar", seen:true, timestamp: 3818107279}

### Required and forbidden fields

Required fields are necessary to store a request. If they are not found in the `POST` body, the request will `400`.

Forbidden fields are used to honeypot spammers. In the frontend, one may include them in the form but hide them,
making it so only the bots complete them. If one of the forbidden field is found, the contact request returns `204`,
but discards all data.

## Appointments

Appointments are stored in the database and sync'ed to Google Calendar.
All appointment endpoints require authentication.

	POST /carm/v1/api/appointment
	Accepts: an Appointment: {
		host: string,
		customerName: string,
		startTime: timestamp,
		endTime: timestamp,
		price: number,
		currency: ISO 4217 string,
		photographer: string enum,
		photoshootType: string,
		photoshootPackage: string,
	}
	Example: {
		host: string,
		customerName: "Michael McDoesntExist",
		startTime: 3821947200,
		endTime: 3821952600,
		price: 275.50
		currency: "EUR",
		photographer: "Thomas Example III",
		photoshootType: "outdoors",
		photoshootPackage: "forest"
	}
	Returns: 204 NO CONTENT or 400 BAD REQUEST

## Google Integration

To set up Google integration you need to create a Google service account and fill in the details in `carm.conf`:

	googleIamServiceEmail: "iam.example@example.com",
	googleRsaKeyPath: "./keyrsa.pem",

### Google Calendar Integration

### Google Spreadsheets Integration

All non-spam contact requests are processed to a google spreadsheet on a 1-minute poll. No API actions happen if there
are no processable requests.

To set up GSheets integration, you need to modify three parameters in `carm.conf`:

	contactRequestSpreadsheetId: "#spreadsheetId here#",
	contactRequestSheetNames: {"example.com": "Sheet1" },

* `googleRsaKeyPath` is a path to a file relative to `carm.conf` containing a google service account RSA key. These are
generated using Google's online tools
* `contactRequestSpreadsheetId` is the spreadsheet ID (available in the URL) of the "master" spreadsheet where contact requests
are uploaded
* `contactRequestSheetNames` is a `dict` mapping each TLD (the `host` field in `POST /contact-request`) to a Sheet in the Spreadsheet.
