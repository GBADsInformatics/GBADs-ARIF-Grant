CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    user_firstname VARCHAR(50) NOT NULL,
    user_lastname VARCHAR(50) NOT NULL,
    user_email VARCHAR(100) UNIQUE NOT NULL,
    user_country VARCHAR(100) NOT NULL,
    user_language VARCHAR(100) NOT NULL,
    user_role VARCHAR(50) NOT NULL
)

CREATE models (
    model_id SERIAL PRIMARY KEY,
    model_name VARCHAR(50) NOT NULL,
    model_version 
)