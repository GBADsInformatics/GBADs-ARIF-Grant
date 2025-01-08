import os
import boto3
from fastapi import FastAPI, File, Form, UploadFile, HTTPException, Response
from fastapi.responses import StreamingResponse
from botocore.exceptions import NoCredentialsError

# Used code from https://github.com/guillelpnz/fast-api-s3-file-manager/blob/main/main.py as inspiration/a starting point

app = FastAPI()

# FIXME - use secrets for this? 
aws_access_key = os.getenv('AWS_ACCESS_KEY_ID')
aws_secret_key = os.getenv('AWS_SECRET_ACCESS_KEY')

s3 = boto3.client('s3', aws_access_key_id=aws_access_key, aws_secret_access_key=aws_secret_key)

@app.post("/upload")
async def upload_file(bucket_name: str = Form(...), object_name: str = Form(...), file: UploadFile = File(...)):
    try:
        s3.upload_fileobj(file.file, bucket_name, object_name)
        return {"message": "File uploaded successfully to S3"}
    except NoCredentialsError:
        raise HTTPException(status_code=500, detail="AWS credentials not available")

@app.get("/download")
async def download_file(bucket_name: str, object_name: str):
    try:
        response = s3.get_object(Bucket=bucket_name, Key=object_name)

        return StreamingResponse(content=response["Body"].iter_chunks())

    except NoCredentialsError:
        raise HTTPException(status_code=500, detail="AWS credentials not available")
    except s3.exceptions.NoSuchKey as e:
        raise HTTPException(status_code=404, detail=f"Object not found: {object_name}")
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"An error occurred: {str(e)}")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)