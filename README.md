# Dynamic Dashboard Data Pipeline

This repository contains the code and documentation for a data pipeline that processes input files, converts them into YAML format, and makes them available for the Global Burden of Animal Diseases (GBADs) Dynamic Population Model (DPM). 

Once the model produces output files, these are stored and made accessible for a dashboard developer.

---

## Project Workflow

### **Overview**
1. **Input Handling**: Process Excel spreadsheets.
2. **Data Conversion**: Convert the data to YAML format.
3. **Model Integration**: Provide the YAML files to the model.
4. **Output Handling**: Store the model's output files and make them available for the dashboard developer.

### **User Flow Diagram**

There are two "versions" of how users will be expected to interact with the system. 

#### Online Version 

![SystemDiagram](diagrams/userFlowDiagramOnlineVersion.png)

#### Desktop Version 

Diagram to be added. This version assumes no internet connection.

---

## Repository Structure

```
.
├── input_files/            # Directory for Excel input files
├── output_files/           # Directory for storing model output files
├── scripts/                # Python scripts for each pipeline step
│   ├── convert_to_yaml.py  # Converts Excel data to YAML
│   ├── upload_to_s3.py     # Uploads YAML files to AWS S3
│   ├── api_endpoint.py     # Serves files via an API
├── requirements.txt        # Python dependencies
├── diagrams/               # Diagrams for documentation
├── README.md               # Project documentation
```

Note that the repository structure includes a directory for input and output files; these are for tests. Actual input and output files will be stored in S3. 

---
## API Usage

### **Endpoint: Get YAML File**

FIXME: To be added; 

- URL:
- Method:
- Description:

Example Request:


---

## Future Enhancements
- Add error handling and logging for all scripts.
- Automate the pipeline using Apache Airflow (?)

---

## Contributors
- Kassy Raymond 

This GitHub template was generated using ChatGPT.

---

## License
This project is licensed under the [GNU General Public License](LICENSE).

