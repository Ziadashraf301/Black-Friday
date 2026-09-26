#!/bin/bash
set -e
set -u

function create_user_and_database() {
    local database=$1
    local user=$2
    local password=$3
    echo "Creating user '$user' and database '$database'..."
    psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
        DO
        \$do\$
        BEGIN
            IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = '$user') THEN
                CREATE USER $user WITH ENCRYPTED PASSWORD '$password';
            END IF;
        END
        \$do\$;
        SELECT 'CREATE DATABASE $database OWNER $user'
        WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '$database')\gexec
        GRANT ALL PRIVILEGES ON DATABASE $database TO $user;
EOSQL
}

if [ -n "${APP_DB_NAME:-}" ]; then
    create_user_and_database "$APP_DB_NAME" "$POSTGRES_USER" "$POSTGRES_PASSWORD"
fi

if [ -n "${MLFLOW_DB_NAME:-}" ]; then
    create_user_and_database "$MLFLOW_DB_NAME" "${MLFLOW_DB_USER}" "${MLFLOW_DB_PASSWORD}"
fi

echo "Multi-database initialization complete."
