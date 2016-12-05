## GET /xandar/users

#### Authentication



Clients must supply the following data


#### GET Parameters:

- include
     - **Values**: *name, profile.description*
     - **Description**: a list of fields to be included in the objects returned
     - This parameter is a **list**. All GET parameters with the name include[] will forward their values in a list to the handler.

- where
     - **Values**: *(name eq 'Sherlock') or (profile.date lt 12999888)*
     - **Description**: record filters

- sort
     - **Values**: *+name, -createdAt*
     - **Description**: sort fields (prefix with '-' for descending sort)
     - This parameter is a **list**. All GET parameters with the name sort[] will forward their values in a list to the handler.

- start
     - **Values**: *0*
     - **Description**: start index for pagination

- limit
     - **Values**: *50*
     - **Description**: the number of records to return (page size)


#### Response:

- Status code 200
- Headers: [("Link",""),("X-Total-Count","")]

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"id":"123","isadmin":true,"email":"milky@way.com"}]
```

- 

```javascript
[{"id":"123","isadmin":true,"email":"milky@way.com"},{"id":"123","isadmin":true,"email":"milky@way.com"}]
```

- 

```javascript
[{"id":"123","isadmin":true,"email":"milky@way.com"},{"id":"123","isadmin":true,"email":"milky@way.com"},{"id":"123","isadmin":true,"email":"milky@way.com"}]
```

- 

```javascript
[{"id":"123","isadmin":true,"email":"milky@way.com"},{"id":"123","isadmin":true,"email":"milky@way.com"},{"id":"123","isadmin":true,"email":"milky@way.com"},{"id":"123","isadmin":true,"email":"milky@way.com"}]
```

## HEAD /xandar/users

#### Authentication



Clients must supply the following data


#### Response:

- Status code 204
- Headers: [("ETag","")]

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## OPTIONS /xandar/users

#### Authentication



Clients must supply the following data


#### Response:

- Status code 204
- Headers: [("Allow","")]

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## PATCH /xandar/users

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
[]
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"email":"milky@way.com","id":"123","isadmin":true}]
```

- 

```javascript
[{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true}]
```

- 

```javascript
[{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true}]
```

- 

```javascript
[{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true}]
```

## POST /xandar/users

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"id":"123","isadmin":true,"email":"milky@way.com"}
```

#### Response:

- Status code 201
- Headers: [("Location","")]

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"id":"123","isadmin":true,"email":"milky@way.com"}
```

## PUT /xandar/users

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
[]
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"email":"milky@way.com","id":"123","isadmin":true}]
```

- 

```javascript
[{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true}]
```

- 

```javascript
[{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true}]
```

- 

```javascript
[{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true},{"email":"milky@way.com","id":"123","isadmin":true}]
```

## DELETE /xandar/users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: user identifier

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## GET /xandar/users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: user identifier

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"id":"123","isadmin":true,"email":"milky@way.com"}
```

## HEAD /xandar/users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: user identifier

#### Response:

- Status code 204
- Headers: [("ETag",""),("Last-Modified","")]

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## OPTIONS /xandar/users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: user identifier

#### Response:

- Status code 204
- Headers: [("Allow","")]

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## PATCH /xandar/users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: user identifier

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"id":"123","isadmin":true,"email":"milky@way.com"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"id":"123","isadmin":true,"email":"milky@way.com"}
```

## PUT /xandar/users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: user identifier

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"id":"123","isadmin":true,"email":"milky@way.com"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"id":"123","isadmin":true,"email":"milky@way.com"}
```

