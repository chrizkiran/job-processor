# Job Processor

HTTP service for processing jobs with task dependencies. Performs topological sorting to determine proper execution order and returns both JSON response and bash script representation.

## Features

- **Dependency Resolution**: Automatically sorts tasks based on dependencies using topological sorting
- **Dual Output Format**: Returns both JSON task list and executable bash script
- **Error Handling**: Detects missing dependencies, circular dependencies, and invalid formats
- **HTTP API**: RESTful endpoints with proper content negotiation
- **Content Types**: Supports both `application/json` and `text/plain` responses

## Requirements

- Erlang/OTP 25+ 
- rebar3

## Build

```bash
rebar3 compile
```

## Run

Start the HTTP server on port 8080:

```bash
rebar3 shell
```

The server will automatically start and listen on `http://localhost:8080`

## API Endpoints

### POST /process

Process a job with tasks and dependencies.

**Request:**
- **Content-Type:** `application/json`
- **Accept:** `application/json` (default) or `text/plain` (bash script only)

**Request Body:**
```json
{
  "tasks": [
    {
      "name": "task-1",
      "command": "touch /tmp/file1"
    },
    {
      "name": "task-2", 
      "command": "cat /tmp/file1",
      "requires": ["task-3"]
    },
    {
      "name": "task-3",
      "command": "echo 'Hello World!' > /tmp/file1", 
      "requires": ["task-1"]
    },
    {
      "name": "task-4",
      "command": "rm /tmp/file1",
      "requires": ["task-2", "task-3"]
    }
  ]
}
```

**Response (default - application/json):**
```json
{
  "tasks": [
    {"name": "task-1", "command": "touch /tmp/file1"},
    {"name": "task-3", "command": "echo 'Hello World!' > /tmp/file1"},
    {"name": "task-2", "command": "cat /tmp/file1"},
    {"name": "task-4", "command": "rm /tmp/file1"}
  ],
  "bash_script": "#!/usr/bin/env bash\ntouch /tmp/file1\necho 'Hello World!' > /tmp/file1\ncat /tmp/file1\nrm /tmp/file1\n"
}
```

### GET /health

Health check endpoint.

**Response:**
```json
{
  "status": "ok",
  "service": "job_processor", 
  "timestamp": 1761505347
}
```

## Usage Examples

### 1. Process Job (Get Both JSON and Bash Script)

```bash
curl -X POST http://localhost:8080/process \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      {"name": "task-1", "command": "touch /tmp/file1"},
      {"name": "task-2", "command": "cat /tmp/file1", "requires": ["task-3"]},
      {"name": "task-3", "command": "echo '\''Hello World!'\'' > /tmp/file1", "requires": ["task-1"]},
      {"name": "task-4", "command": "rm /tmp/file1", "requires": ["task-2", "task-3"]}
    ]
  }'
```

### 2. Get Bash Script Only

```bash
curl -X POST http://localhost:8080/process \
  -H "Content-Type: application/json" \
  -H "Accept: text/plain" \
  -d '{
    "tasks": [
      {"name": "hello", "command": "echo '\''Hello World!'\''"}
    ]
  }'
```

### 3. Extract Bash Script from JSON Response

```bash
curl -s -X POST http://localhost:8080/process \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      {"name": "hello", "command": "echo '\''Hello World!'\''"}
    ]
  }' | jq -r '.bash_script'
```

### 4. Save Bash Script to File and Execute

```bash
curl -s -X POST http://localhost:8080/process \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      {"name": "setup", "command": "mkdir -p /tmp/demo"},
      {"name": "create", "command": "echo '\''Hello World!'\'' > /tmp/demo/test.txt", "requires": ["setup"]},
      {"name": "read", "command": "cat /tmp/demo/test.txt", "requires": ["create"]},
      {"name": "cleanup", "command": "rm -rf /tmp/demo", "requires": ["read"]}
    ]
  }' | jq -r '.bash_script' > script.sh

chmod +x script.sh
./script.sh
```

### 5. Health Check

```bash
curl http://localhost:8080/health
```

## Error Handling

The service returns appropriate HTTP status codes and error messages:

### Missing Dependency (400)
```bash
curl -X POST http://localhost:8080/process \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      {"name": "task-1", "command": "echo hello", "requires": ["missing-task"]}
    ]
  }'
```

**Response:**
```json
{
  "error": "missing_dependency",
  "message": "Missing dependency: missing-task"
}
```

### Circular Dependency (400)
```bash
curl -X POST http://localhost:8080/process \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      {"name": "task-1", "command": "echo 1", "requires": ["task-2"]},
      {"name": "task-2", "command": "echo 2", "requires": ["task-1"]}
    ]
  }'
```

**Response:**
```json
{
  "error": "circular_dependency", 
  "message": "Circular dependency detected"
}
```

## Development

### Interactive Testing

You can also test the core logic directly in the Erlang shell:

```erlang
% Start the shell
rebar3 shell

% Test the core processor
TestJson = <<"{\"tasks\":[{\"name\":\"hello\",\"command\":\"echo 'Hello World!'\"}]}">>.
job_processor:process_job(TestJson).

% Pretty print results  
job_processor_helper:pretty_print(job_processor:process_job(TestJson)).

% Run demo
job_processor_helper:demo().
```

### Project Structure

```
src/
├── job_processor.erl          % Core business logic
├── job_processor_handler.erl  % HTTP request handler  
├── job_processor_helper.erl   % Pretty printing utilities
├── job_processor_sup.erl      % OTP supervisor
├── job_processor_app.erl      % OTP application
└── job_processor.app.src      % Application specification
```

## Algorithm

The service uses **topological sorting** with Erlang's built-in `digraph` library to resolve task dependencies:

1. **Parse and validate** JSON input
2. **Check dependency existence** - ensure all required tasks exist
3. **Build dependency graph** - create directed graph of task relationships  
4. **Topological sort** - determine execution order, detect cycles
5. **Generate responses** - create both JSON and bash script formats

## License

Apache-2.0
