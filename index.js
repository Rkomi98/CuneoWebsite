const express = require('express');
const PDFParser = require('pdf-parse');
const fs = require('fs');
const app = express();
const uploadDirectory = './uploads';
const multer = require('multer');
const path = require('path');
const mammoth = require('mammoth');
const pdf = require('html-pdf');

app.use(express.json());
app.use(express.urlencoded({ extended: true }));

const storage = multer.diskStorage({
       destination: (req, file, cb) => {
          cb(null, 'uploads/');
       },
       filename: (req, file, cb) => {
          cb(null, file.originalname);
       }
   });
const upload = multer({ storage });

app.post('/upload', upload.single('file'), async (req, res) => {
       // File upload logic goes here
   });

app.listen(3000, () => {
    console.log('Server is running on port 3000');
 });

app.post('/upload', upload.single('file'), async (req, res) => {
    try {
        // Check if a file was uploaded
        if (!req.file) {
            return res.status(400).json({ error: 'No file uploaded' });
        }

        // Retrieve the uploaded file from the request body
        const uploadedFile = req.file;

        // Write the file to the upload directory
        const fileName = `${uploadedFile.originalname}`;
        const filePath = `${uploadDirectory}/${fileName}`;
        const fileData = fs.readFileSync(filePath, 'utf8');
        await processFileData(fileData);

        // Determine the file type
        const fileExtension = uploadedFile.mimetype ?uploadedFile.mimetype : null;

        // Check if the file is already in PDF format
        if (fileExtension === 'application/pdf') {
            // Process the PDF directly
            await processPDF(filePath, res);
        } else {
            // Convert the file to PDF
            const convertedFilePath = await convertToPDF(filePath);

            // Process the converted PDF
            await processPDF(convertedFilePath, res);
        }
    } catch (error) {
         console.error('An error occurred while processing the file:', error);
         res.status(500).json({ error: 'Failed to process the file' });
    }
});