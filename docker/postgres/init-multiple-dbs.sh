#!/bin/bash
set -e
set -u

function create_user_and_database() {
    local database=$1
    local user=$2
    local password=$3
    echo "Creating user '$user' and database '$database'..."
    # Names and the password go in as psql variables (:'x' = quoted literal) and are quoted by format(%I / %L),
    # so quotes or other special characters in them cannot break out of the SQL.
    psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" -v db="$database" -v usr="$user" -v pw="$password" <<-'EOSQL'
		SELECT format('CREATE USER %I WITH ENCRYPTED PASSWORD %L', :'usr', :'pw')
		WHERE NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = :'usr')\gexec
		SELECT format('CREATE DATABASE %I OWNER %I', :'db', :'usr')
		WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = :'db')\gexec
		SELECT format('GRANT ALL PRIVILEGES ON DATABASE %I TO %I', :'db', :'usr')\gexec
EOSQL
}

if [ -n "${APP_DB_NAME:-}" ]; then
    create_user_and_database "$APP_DB_NAME" "$POSTGRES_USER" "$POSTGRES_PASSWORD"
fi

if [ -n "${MLFLOW_DB_NAME:-}" ]; then
    create_user_and_database "$MLFLOW_DB_NAME" "${MLFLOW_DB_USER}" "${MLFLOW_DB_PASSWORD}"
fi

echo "Multi-database initialization complete."
