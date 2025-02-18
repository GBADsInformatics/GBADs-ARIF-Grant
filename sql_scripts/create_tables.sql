CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    user_firstname VARCHAR(50) NOT NULL,
    user_lastname VARCHAR(50) NOT NULL,
    user_email VARCHAR(100) UNIQUE NOT NULL,
    user_country VARCHAR(100) NOT NULL,
    user_language VARCHAR(100) NOT NULL,
    user_role VARCHAR(50) NOT NULL
)

CREATE TABLE user_dashboards (
    dashboard_id SERIAL PRIMARY KEY,
    user_id SERIAL REFERENCES users(user_id) NOT NULL,
    user_model_id SERIAL REFERENCES user_models(user_model_id) NOT NULL,
    date_created TIMESTAMP NOT NULL,
    date_updated TIMESTAMP NOT NULL,
    dashboard_s3_url TEXT NOT NULL,
    dashboard_name VARCHAR(100) NOT NULL
)

CREATE TABLE user_models (
    user_id SERIAL REFERENCES users(user_id) NOT NULL,
    user_model_id SERIAL PRIMARY KEY,
    user_model_name VARCHAR(100) NOT NULL,
    user_model_status VARCHAR(100) NOT NULL,
    user_model_summary_s3_url TEXT NOT NULL,
    user_model_full_s3_url TEXT NOT NULL,
    user_model_yaml_s3_url TEXT NOT NULL,
    date_created TIMESTAMP NOT NULL,
    date_completed TIMESTAMP NOT NULL
)
