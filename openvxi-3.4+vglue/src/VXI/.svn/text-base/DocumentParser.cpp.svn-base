
/****************License************************************************
 * Vocalocity OpenVXI
 * Copyright (C) 2004-2005 by Vocalocity, Inc. All Rights Reserved.
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * Vocalocity, the Vocalocity logo, and VocalOS are trademarks or 
 * registered trademarks of Vocalocity, Inc. 
 * OpenVXI is a trademark of Scansoft, Inc. and used under license 
 * by Vocalocity.
 ***********************************************************************/

#include "DocumentParser.hpp"
#include "DocumentStorage.hpp"
#include "CommonExceptions.hpp"
#include "SimpleLogger.hpp"
#include "VXML.h"                    // for attribute names
#include "DocumentConverter.hpp"     // for DocumentConverter
#include "PropertyList.hpp"
#include "XMLChConverter.hpp"
#include "ValueLogging.hpp"     // for VXIValue dumping
#include "VXIinet.h"

// Internal documents
#include "Schema.hpp"

// Xerces related
#ifndef HAVE_XERCES
#error Need Apache Xerces to build the VoiceXML interpreter
#endif

#include <util/PlatformUtils.hpp>
#include <util/TransService.hpp>
#include <sax2/XMLReaderFactory.hpp>
#include <framework/MemBufInputSource.hpp>
#include <sax/SAXParseException.hpp> // by ErrorReporter
#include <sax/EntityResolver.hpp>    // by DTDResolver
#include <sax/ErrorHandler.hpp>      // by ErrorReporter
#include <validators/common/Grammar.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMImplementationLS.hpp>
#include <xercesc/dom/DOMImplementationRegistry.hpp>
#include <xercesc/dom/DOMBuilder.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMErrorHandler.hpp>
#include <xercesc/dom/DOMLocator.hpp>
#include <xercesc/framework/Wrapper4InputSource.hpp>

#include <syslog.h>
#include <vglue_tostring.h>
#include <vglue_ipc.h>

using namespace xercesc;

//#############################################################################
// Utilities - these are specific to Xerces
//#############################################################################

class ErrorReporter : public ErrorHandler {
public:
  ErrorReporter()  { }
  ~ErrorReporter() { }
  
  void warning(const SAXParseException& toCatch)     { /* Ignore */ }
  void fatalError(const SAXParseException& toCatch)  { error(toCatch); }
  void resetErrors() { }

  void error(const SAXParseException & toCatch)
  { throw SAXParseException(toCatch); }

private:
  ErrorReporter(const ErrorReporter &);
  void operator=(const ErrorReporter &);
};

class VXIDOMException
{
public:
  VXIDOMException(const DOMError& domError) {
    XMLChToVXIchar uri(domError.getLocation()->getURI());
    XMLChToVXIchar message(domError.getMessage());

    _uri = uri.c_str();
    _message = message.c_str();
    _line = domError.getLocation()->getLineNumber();
    _col = domError.getLocation()->getColumnNumber();
  }

  vxistring getURI() const { return _uri; }
  vxistring getMessage() const { return _message; }
  int getLine() const { return _line; }
  int getColumn() const { return _col; }

private:
  vxistring _uri;
  vxistring _message;
  int       _line;
  int       _col;
};

class DOMErrorReporter : public DOMErrorHandler
{
public:
	DOMErrorReporter(){}
	~DOMErrorReporter(){}

	bool handleError(const DOMError& domError) {
		throw VXIDOMException(domError);
		return true;
	}

private :
    DOMErrorReporter(const DOMErrorReporter&);
    void operator=(const DOMErrorReporter&);
};

class DTDResolver : public EntityResolver {
public:
  virtual ~DTDResolver() { }
  DTDResolver() { }

  virtual InputSource * resolveEntity(const XMLCh * const publicId,
                                      const XMLCh * const systemId)
  {

    if (publicId && Compare(publicId, L"SB_Defaults")) {
      VXIcharToXMLCh name(L"VXML Defaults DTD (for SB 1.0)");
      return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_DEFAULTS_DTD,
                                   VALIDATOR_DEFAULTS_DTD_SIZE,
                                   name.c_str(), false);
    }
    
    if( systemId ) {
      if (Compare(systemId, L"http://www.w3.org/TR/voicexml20/vxml.xsd")) {
        VXIcharToXMLCh name(L"http://www.w3.org/TR/voicexml21/vxml.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_VXML,
                                     VALIDATOR_VXML_SIZE,
                                     name.c_str(), false);
      }

      if (Compare(systemId, L"http://www.w3.org/TR/voicexml21/vxml.xsd")) {
        VXIcharToXMLCh name(L"http://www.w3.org/TR/voicexml21/vxml.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_VXML,
                                     VALIDATOR_VXML_SIZE,
                                     name.c_str(), false);
      }

	  if (Compare(systemId, L"vxml-datatypes.xsd")) {
        VXIcharToXMLCh name(L"vxml-datatypes.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_VXML_DATA,
                                     VALIDATOR_VXML_DATA_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"vxml-attribs.xsd")) {
        VXIcharToXMLCh name(L"vxml-attribs.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_VXML_ATTR,
                                     VALIDATOR_VXML_ATTR_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"vxml-grammar-extension.xsd")) {
        VXIcharToXMLCh name(L"vxml-grammar-extension.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_SRGF_EXTN,
                                     VALIDATOR_SRGF_EXTN_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"vxml-grammar-restriction.xsd")) {
        VXIcharToXMLCh name(L"vxml-grammar-restriction.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_SRGF_RSTR,
                                     VALIDATOR_SRGF_RSTR_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"vxml-synthesis-extension.xsd")) {
        VXIcharToXMLCh name(L"vxml-synthesis-extension.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_SSML_EXTN,
                                     VALIDATOR_SSML_EXTN_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"vxml-synthesis-restriction.xsd")) {
        VXIcharToXMLCh name(L"vxml-synthesis-restriction.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_SSML_RSTR,
                                     VALIDATOR_SSML_RSTR_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"synthesis-core.xsd")) {
        VXIcharToXMLCh name(L"synthesis-core.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_SSML_CORE,
                                     VALIDATOR_SSML_CORE_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"grammar-core.xsd")) {
        VXIcharToXMLCh name(L"grammar-core.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_SRGF_CORE,
                                     VALIDATOR_SRGF_CORE_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"http://www.w3.org/2001/xml.xsd")) {
        VXIcharToXMLCh name(L"http://www.w3.org/2001/xml.xsd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_XML,
                                     VALIDATOR_XML_SIZE,
                                     name.c_str(), false);
      }
  
      if (Compare(systemId, L"XMLSchema.dtd")) {
        VXIcharToXMLCh name(L"XMLSchema.dtd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_SCHEMA_DTD,
                                     VALIDATOR_SCHEMA_DTD_SIZE,
                                     name.c_str(), false);
      }
      
      if (Compare(systemId, L"datatypes.dtd")) {
        VXIcharToXMLCh name(L"datatypes.dtd (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_DATATYPE_DTD,
                                     VALIDATOR_DATATYPE_DTD_SIZE,
                                     name.c_str(), false);
      }
  
      if ( ( publicId && 
		  Compare(publicId, L"-//W3C//DTD VOICEXML 2.0//EN")  ||
		  Compare(publicId, L"-//W3C//DTD VOICEXML 2.1//EN")) ||
          Compare(systemId, L"http://www.w3.org/TR/voicexml20/vxml.dtd") || 
		  Compare(systemId, L"http://www.w3.org/TR/voicexml21/vxml.dtd"))
      {
        VXIcharToXMLCh name(L"VXML DTD (SB)");
        return new MemBufInputSource(VALIDATOR_DATA + VALIDATOR_VXML_DTD,
                                     VALIDATOR_VXML_DTD_SIZE,
                                     name.c_str(), false);
      }
    }
    // If the id is not supported by VXI, return NULL so that
    // xerces will fetch the id itself
    return NULL;
  }
};

//#############################################################################
// Document Parser
//#############################################################################

// xerces crashes when on multi-thread app. that simultaneously load schema
// grammar therefore use a global mutext to restrict access to only thread
// at a time
static VXItrdMutex* gblXMLGrammarMutex = NULL;

bool DocumentParser::Initialize(unsigned int cacheSize)
{
  try {
    VXItrdMutexCreate(&gblXMLGrammarMutex);

    //  Log to voiceglue
    if (voiceglue_loglevel() >= LOG_INFO)
    {
	std::ostringstream logstring;
	logstring << "DocumentParser: XMLPlatformUtils::Initialize()";
	voiceglue_log ((char) LOG_INFO, logstring);
    };

    XMLPlatformUtils::Initialize();
    if (!VXMLDocumentModel::Initialize()) return false;
    DocumentConverter::Initialize();
  }

  catch (const XMLException &) {
    return false;
  }

  DocumentStorageSingleton::Initialize(cacheSize);

  return true;
}


void DocumentParser::Deinitialize()
{
  DocumentStorageSingleton::Deinitialize();

  try {
    VXItrdMutexDestroy(&gblXMLGrammarMutex);
    DocumentConverter::Deinitialize();
    VXMLDocumentModel::Deinitialize();

    //  Log to voiceglue
    if (voiceglue_loglevel() >= LOG_INFO)
    {
	std::ostringstream logstring;
	logstring << "DocumentParser: XMLPlatformUtils::Terminate()";
	voiceglue_log ((char) LOG_INFO, logstring);
    };

    XMLPlatformUtils::Terminate();
  }
  catch (const XMLException &) {
    // do nothing
  }
}

static void LockLoadGrammar(void)
{
  if( gblXMLGrammarMutex ) VXItrdMutexLock(gblXMLGrammarMutex);
}

static void UnlockLoadGrammar(void)
{
  if( gblXMLGrammarMutex ) VXItrdMutexUnlock(gblXMLGrammarMutex);
}  

DocumentParser::DocumentParser()
  : parser(NULL), converter(NULL), loadedVXML20(false)
{
  converter = new DocumentConverter();
  if (converter == NULL) throw VXIException::OutOfMemory();

  parser = XMLReaderFactory::createXMLReader();
  if (parser == NULL) {
    delete converter;
    throw VXIException::OutOfMemory();
  }

  DTDResolver * dtd = new DTDResolver();
  if (dtd == NULL) {
    delete converter;
    delete parser;
    throw VXIException::OutOfMemory();
  }
  parser->setEntityResolver(dtd);

  // These settings below should not change the Xerces defaults.  Their
  // presence makes the defaults explicit.

  parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);
//  parser->setFeature(XMLUni::fgSAX2CoreValidation, true);
  parser->setFeature(XMLUni::fgSAX2CoreValidation, false);
  parser->setFeature(XMLUni::fgXercesDynamic, false);
  parser->setFeature(XMLUni::fgXercesSchema, true);
  parser->setFeature(XMLUni::fgXercesSchemaFullChecking, true);
  parser->setFeature(XMLUni::fgSAX2CoreNameSpacePrefixes, false);

  ErrorHandler *errReporter = new ErrorReporter();
  parser->setErrorHandler(errReporter);

  parser->setContentHandler(converter);
  parser->setLexicalHandler(converter);

  // this is rather wasteful... but the only way to get
  // the document encoding.  this is only done so that
  // integrators could convert grammars or ssml docs
  // back to the original encoding if needed.
  parser->installAdvDocHandler(this);
}


DocumentParser::~DocumentParser()
{
  if (parser != NULL) {
    const ErrorHandler * reporter = parser->getErrorHandler();
    delete reporter;

    const EntityResolver * resolver = parser->getEntityResolver();
    delete resolver;

    delete parser;
    delete converter;
    parser = NULL;
  }

  while(!domParsers.empty()){
    xercesc::DOMBuilder *domParser = domParsers.front();
	domParsers.pop_front();
	DOMErrorReporter *domReporter = (DOMErrorReporter *) domParser->getErrorHandler();
    if(domReporter) delete domReporter;
    domParser->release();
  }
}


//****************************************************************************
// FetchBuffer
//****************************************************************************

// 1: Invalid parameter
// 2: Unable to open URL
// 3: Unable to read from URL
int DocumentParser::FetchBuffer(const VXIchar * url,
                                const VXIMapHolder & properties,
                                VXIMapHolder & streamInfo,
                                VXIinetInterface * inet,
                                SimpleLogger & log,
                                const VXIbyte * & result,
                                VXIulong & read,
                                vxistring & docURL)
{
  if (log.IsLogging(2)) {
    log.StartDiagnostic(2) << L"DocumentParser::FetchBuffer(" << url
                           << L", " << properties.GetValue() << L")";
    log.EndDiagnostic();
  }

  // Set url for error report
  log.SetUri( url ? url : L"NONE" );
  
  if (inet == NULL || url == NULL || wcslen(url) == 0) return 1;
    
  // (1) Open URL
  VXIinetStream * stream;

  // VXIMapHolder streamInfo;
  if (streamInfo.GetValue() == NULL) 
  {
    return -1;
  }
  
  if (inet->Open(inet, L"vxi", url, INET_MODE_READ, 0, properties.GetValue(),
                 streamInfo.GetValue(), &stream) != 0)
  {
    if (log.IsLogging(0)) {
      log.StartDiagnostic(0) << L"DocumentParser::FetchBuffer - could not "
        L"open URL: " << url;
      log.EndDiagnostic();
    }
    return 2;
  }
  
  // (2) Determine document size & read into local memory buffer.
  const VXIValue * tempURL = NULL;
  tempURL = VXIMapGetProperty(streamInfo.GetValue(), INET_INFO_ABSOLUTE_NAME);
  if (tempURL == NULL || VXIValueGetType(tempURL) != VALUE_STRING) {
    inet->Close(inet, &stream);
    if (log.IsLogging(0)) {
      log.StartDiagnostic(0) << L"DocumentParser::FetchBuffer - could not "
        L"retrieve absolute path of document at URL: " << url;
      log.EndDiagnostic();
    }
    return 2;
  }
  docURL = VXIStringCStr(reinterpret_cast<const VXIString *>(tempURL));

  const VXIValue * tempSize = NULL;
  tempSize = VXIMapGetProperty(streamInfo.GetValue(), INET_INFO_SIZE_BYTES);
  if (tempSize == NULL || VXIValueGetType(tempSize) != VALUE_INTEGER) {
    inet->Close(inet, &stream);
    if (log.IsLogging(0)) {
      log.StartDiagnostic(0) << L"DocumentParser::FetchBuffer - could not "
        L"retrieve size of document at URL: " << url;
      log.EndDiagnostic();
    }
    return 2;
  }

  VXIint32 bufSize
    = VXIIntegerValue(reinterpret_cast<const VXIInteger *>(tempSize));

  if (bufSize < 2047)
    bufSize = 2047;

  ++bufSize;
  VXIbyte * buffer = new VXIbyte[bufSize];
  if(buffer == NULL) {
    log.LogError(202);
    return -1;
  }

  bool reachedEnd = false; 
  read = 0;

  while (!reachedEnd) {
    VXIulong bytesRead = 0;
    switch (inet->Read(inet, buffer+read, bufSize-read, &bytesRead, stream)) {
    case VXIinet_RESULT_SUCCESS:
      read += bytesRead;
      break;
    case VXIinet_RESULT_END_OF_STREAM:
      read += bytesRead;
      reachedEnd = true;  // exit while
      break;
    case VXIinet_RESULT_WOULD_BLOCK:
      VXItrdThreadYield();
      break;
    default:
      inet->Close(inet, &stream);
      delete[] buffer;

      log.LogDiagnostic(0, L"DocumentParser::FetchBuffer - "
                        L"could not read from URL.");
      return 3;
    }

    if (read == static_cast<VXIulong>(bufSize)) {
      // The number of bytes read exceeds the number expected.  Double the
      // size and keep reading.
      VXIbyte * temp = new VXIbyte[2*bufSize];
      if(temp == NULL) {
        log.LogError(202);
        delete [] buffer;
        return -1;
      }
      memcpy(static_cast<void *>(temp), static_cast<void *>(buffer),
             bufSize * sizeof(VXIbyte));
      delete[] buffer;
      buffer = temp;
      bufSize *= 2;
    }
  }

  inet->Close(inet, &stream);
  result = buffer;

  log.LogDiagnostic(2, L"DocumentParser::FetchBuffer - success");

  return 0;
}


void DocumentParser::ReleaseBuffer(const VXIbyte * & buffer)
{
  if (buffer != VALIDATOR_DATA + VXML_DEFAULTS)
    delete[] const_cast<VXIbyte *>(buffer);

  buffer = NULL;
}


//#############################################################################
// FetchDocument
//#############################################################################

// -2: Internal error
int DocumentParser::FetchDocument(const VXIchar * url,
                                  const VXIMapHolder & properties,
                                  VXIinetInterface * inet,
                                  VXIcacheInterface * cache,
                                  SimpleLogger & log,
                                  VXMLDocument & document,
                                  VXIMapHolder & docProperties,
                                  bool isDefaults,
                                  bool isRootApp,
                                  VXIbyte **content,
                                  VXIulong *size)
{
  int result;

  if (log.IsLogging(2)) {
    log.StartDiagnostic(2) << L"DocumentParser::FetchDocument(" << url << L")";
    log.EndDiagnostic();
  }

  // (1) Load the VXML DTD for validation.  This will override an externally
  // specified DTD if the user provides a link.

  try {
    if (isDefaults) {
      MemBufInputSource membuf(
          VALIDATOR_DATA + DUMMY_VXML_DEFAULTS_DOC,
          DUMMY_VXML_DEFAULTS_DOC_SIZE,
          "vxml 1.0 defaults");
      parser->parse(membuf);
      converter->ResetDocument(); // Throw this document away.
    }

    if (!isDefaults && !loadedVXML20) {
      // Preload the VXML 2.1 schema.
      VXIcharToXMLCh name(L"http://www.w3.org/TR/voicexml21/vxml.xsd");
      LockLoadGrammar();
      parser->loadGrammar(name.c_str(), Grammar::SchemaGrammarType, true);
      // Reuse cached grammars if available.
      parser->setFeature(XMLUni::fgXercesUseCachedGrammarInParse, true);      
      UnlockLoadGrammar(); 
      loadedVXML20 = true;
    }
  }
  catch (const XMLException & exception) {
    if (!isDefaults && !loadedVXML20) 
      UnlockLoadGrammar();  
      
    XMLChToVXIchar message(exception.getMessage());
    log.StartDiagnostic(0) << L"DocumentParser::FetchDocument - XML parsing "
      L"error from DOM: " << message;
    log.EndDiagnostic();
    log.LogError(999, SimpleLogger::MESSAGE, L"unable to load VXML DTD");
    return 4;
  }
  catch (const SAXParseException & exception) {
    if (!isDefaults && !loadedVXML20) 
      UnlockLoadGrammar();  
      
    XMLChToVXIchar sysid(exception.getSystemId());
    XMLChToVXIchar message(exception.getMessage());
    log.StartDiagnostic(0) << L"DocumentParser::FetchDocument - Parse error "
                           << L"in file \"" 
                           << sysid 
                           << L"\", line " << exception.getLineNumber()
                           << L", column " << exception.getColumnNumber()
                           << L" - " << message;
    log.EndDiagnostic();
    log.LogError(999, SimpleLogger::MESSAGE, L"unable to load VXML DTD");
    return 4;
  }
  catch (...) {
    if (!isDefaults && !loadedVXML20) 
      UnlockLoadGrammar();  

    log.StartDiagnostic(0) << L"DocumentParser::FetchDocument - Unknown Parse error";
    log.EndDiagnostic();
    log.LogError(999, SimpleLogger::MESSAGE, L"unable to load VXML DTD");
    return 4;          
  }

  // (2) Load the url into memory.

  const VXIbyte * buffer = NULL;
  VXIulong bufSize = 0;
  vxistring docURL;
  bool isDefaultDoc = false;
  

  if (isDefaults && wcslen(url) == 0) {
    buffer  = VALIDATOR_DATA + VXML_DEFAULTS;
    bufSize = VXML_DEFAULTS_SIZE;
    docURL = L"builtin defaults";
    result = 0;
    isDefaultDoc = true;
  }
  else {
    result = DocumentParser::FetchBuffer(url, properties, docProperties, inet, log,
                                         buffer, bufSize, docURL);
  }

  if (result != 0) {
    if (log.IsLogging(0)) {
      log.StartDiagnostic(0) << L"DocumentParser::FetchDocument - exiting "
        L"with error result " << result;
      log.EndDiagnostic();
    }
    return result; // may return { -1, 1, 2, 3 }
  }

  // store buffer for reference
  if (content) {
    VXIbyte *tempbuf = new VXIbyte[bufSize];
    if(tempbuf == NULL) {
      log.LogError(202);
      if( !isDefaultDoc ) DocumentParser::ReleaseBuffer(buffer);
      return -1;
    }
    memcpy(tempbuf, buffer, bufSize);
    if (size != NULL) *size = bufSize;
    *content = tempbuf;
  }
  
  // (3) Pull the document from cache.
  

  vxistring baseURL;
  VXMLDocument doc;
  if (!DocumentStorageSingleton::Instance()->Retrieve(doc, buffer, bufSize, docURL.c_str())) {
      // (3.1) Set the base uri for this document, but
      // ignore if it is the default doc. Note: only want to do this if
      // not pulling from the cache otherwise the base is messed up
      if(!isDefaultDoc) 
        converter->SetBaseUri(docURL.c_str());

    // (4) Not in cache; parse buffer into our VXML document representation
    try {
      VXIcharToXMLCh membufURL(docURL.c_str());
      // Set Document level
      if( isDefaults ) 
        converter->SetDocumentLevel(DEFAULTS);
      else if( isRootApp )
        converter->SetDocumentLevel(APPLICATION);
      else
        converter->SetDocumentLevel(DOCUMENT);
      // Parse the script
	  MemBufInputSource membuf(buffer, bufSize, membufURL.c_str(), false);
      parser->parse(membuf);
    }
    catch (const XMLException & exception) {
      if( !isDefaultDoc ) DocumentParser::ReleaseBuffer(buffer);
      if (log.IsLogging(0)) {
        XMLChToVXIchar message(exception.getMessage());
        log.StartDiagnostic(0) << L"DocumentParser::FetchDocument - XML "
          L"parsing error from DOM: " << message;
        log.EndDiagnostic();
      }
      return 4;
    }
    catch (const SAXParseException & exception) {
      VXIString *key = NULL, *value = NULL;
      if (log.LogContent(VXI_MIME_XML, buffer, bufSize, &key, &value)) 
      {
        vxistring temp(L"");
        temp += VXIStringCStr(key);
        temp += L": ";
        temp += VXIStringCStr(value);
        log.StartDiagnostic(0) << L"DocumentParser::FetchDocument - buffer is saved in: "
                               << temp;
        log.EndDiagnostic();
      }        
      if (key) VXIStringDestroy(&key);
      if (value) VXIStringDestroy(&value);
      
      if( !isDefaultDoc ) DocumentParser::ReleaseBuffer(buffer);
      if (log.IsLogging(0)) {
        XMLChToVXIchar sysid(exception.getSystemId());
        XMLChToVXIchar message(exception.getMessage());
        log.StartDiagnostic(0) << L"DocumentParser::FetchDocument - Parse "
                               << L"error in file \"" 
                               << sysid
                               << L"\", line " << exception.getLineNumber()
                               << L", column " << exception.getColumnNumber()
                               << L" - " 
                               << message;
        log.EndDiagnostic();
      }
      return 4;
    }

    // (5) Write the compiled form out to the cache.

    doc = converter->GetDocument();
    // Set the current uri as base uri if there isn't a "xml:base" attribute
    doc.GetBaseURL(baseURL);
    if( baseURL.empty() ) doc.SetBaseURL(docURL);
    
    // store default language
    if( isDefaults ) {
      vxistring dlang;
      converter->GetDefaultLang(dlang);
      doc.SetDefaultLang(dlang);
    }        
    // Put into memory cache
    DocumentStorageSingleton::Instance()->Store(doc, buffer, bufSize, docURL.c_str());
  }
  else {
    // The document is already in the memory cache
    // Need to set the base uri
    converter->RestoreBaseURLFromCache(doc);
    // Restore default language
    if( isDefaults ) {
      converter->RestoreDefaultLangFromCache(doc);
    }
  }

  if( !isDefaultDoc ) DocumentParser::ReleaseBuffer(buffer);

  // (6) Parse was successful, process document.  We want only the top level
  // <vxml> node.

  const VXMLElement root = doc.GetRoot();
  VXMLElementType nodeName = root.GetName();

  // If we're looking for the defaults, we can exit early.
  if (isDefaults && nodeName == DEFAULTS_ROOT) {
    log.LogDiagnostic(2, L"DocumentParser::FetchDocument(): Default document - success");
    document = doc;
    return 0;
  }
  else if (nodeName != NODE_VXML) {
    document = VXMLDocument();
    if (log.IsLogging(0)) {
      log.StartDiagnostic(0) << L"DocumentParser::FetchDocument - unable to "
        L"find " << NODE_VXML << L" in document.";
      log.EndDiagnostic();
    }
    return 4;
  }

  vxistring temp;

  // If the properties map is NULL, don't bother with the remaining settings
  if (docProperties.GetValue() != NULL) {
    // Retrieve the properties and put them into the map.

    VXIString * str = NULL;
    temp = docURL;
    str = VXIStringCreate(temp.c_str());
    if (str == NULL) 
      throw VXIException::OutOfMemory();

    VXIMapSetProperty(docProperties.GetValue(), PropertyList::AbsoluteURI,
                      reinterpret_cast<VXIValue *>(str));

    str = VXIStringCreate(converter->GetBaseUri().empty() ? L"" : 
                          converter->GetBaseUri().c_str());
    
    if (str == NULL)
      throw VXIException::OutOfMemory();

    VXIMapSetProperty(docProperties.GetValue(), PropertyList::BaseURI,
                      reinterpret_cast<VXIValue *>(str));

	// store encoding
    str = VXIStringCreate(encoding.empty() ? L"UTF-8" : encoding.c_str());
    if (str == NULL)
      throw VXIException::OutOfMemory();
    VXIMapSetProperty(docProperties.GetValue(), PropertyList::SourceEncoding,
                      reinterpret_cast<VXIValue *>(str));

	// store xml:lang
	if (root.GetAttribute(ATTRIBUTE_XMLLANG, temp)) {
      str = VXIStringCreate(temp.c_str());
      if (str == NULL)
        throw VXIException::OutOfMemory();

      VXIMapSetProperty(docProperties.GetValue(), PropertyList::Language,
                        reinterpret_cast<VXIValue *>(str));
    }
  }

  log.LogDiagnostic(2, L"DocumentParser::FetchDocument(): VXML document - success");

  document = doc;
  return 0;
}


int DocumentParser::FetchContent(const VXIchar * uri,
                                 const VXIMapHolder & properties,
                                 VXIMapHolder & fetchInfo,
                                 VXIinetInterface * inet,
                                 SimpleLogger & log,
                                 const vxistring & encoding,
                                 vxistring & content)
{
  const VXIbyte * buffer;
  VXIulong bufSize;
  vxistring docURL;

  // (1) Retrieve the URI.
  switch (DocumentParser::FetchBuffer(uri, properties, fetchInfo, inet, log,
                                      buffer, bufSize, docURL)) {
  case -1: // Out of memory?
    return -1;
  case  0: // Success
    break;
  case  2: // Unable to open URI
    return 2;
  case  3: // Unable to read from URI
    return 3;
  case  1: // Invalid parameter
  default:
    return 1;
  }

  // (2) Create a transcoder for the requested type.

  VXIcharToXMLCh encName(encoding.c_str());
  XMLTransService::Codes failReason;
  XMLTranscoder* transcoder = 
      XMLPlatformUtils::fgTransService->makeNewTranscoderFor(encName.c_str(),
                                                             failReason,
                                                             8*1064
#if _XERCES_VERSION <= 20200                                                             
                                                              );
#else
                                                             ,XMLPlatformUtils::fgMemoryManager);
#endif

  if (transcoder == NULL) return 4;

  // (3) Allocate memory for the conversion.

  XMLCh * convertedString = new XMLCh[bufSize+1];
  unsigned char* charSizes = new unsigned char[bufSize];

  if (convertedString == NULL || charSizes == NULL) {
    delete[] convertedString;
    delete[] charSizes;
    return -1;
  }

  // (4) Transcode the values into our string.

  unsigned int bytesEaten;
  unsigned int charsDone = transcoder->transcodeFrom(buffer, bufSize,
                                                     convertedString, bufSize,
                                                     bytesEaten, charSizes);

  // (5) Finally convert from XMLCh to VXIchar.
  convertedString[charsDone] = '\0';  // Add terminator. 
  XMLChToVXIchar result(convertedString);

  // (6) Done.  Release memory.

  content = result.c_str();
  delete[] convertedString;
  delete[] charSizes;
  DocumentParser::ReleaseBuffer(buffer);
  delete transcoder;

  return 0;
}

int DocumentParser::FetchXML(const VXIchar * uri, const VXIMapHolder & fetchobj,
                      VXIMapHolder & fetchStatus, 
					  VXIinetInterface * inet, SimpleLogger & log,
					  DOMDocument **doc )
{
  if (log.IsLogging(2)) {
    log.StartDiagnostic(2) << L"DocumentParser::FetchXML(" << uri << L")";
    log.EndDiagnostic();
  }

  const VXIbyte *buffer = NULL;
  VXIulong cbBuffer = 0;
  vxistring docURI;

  int result = DocumentParser::FetchBuffer(
      uri, fetchobj, fetchStatus,
      inet, log, 
      buffer, cbBuffer,
      docURI);

  if (result != 0) {
    if (log.IsLogging(0)) {
      log.StartDiagnostic(0) << L"DocumentParser::FetchXML - exiting "
        L"with error result " << result;
      log.EndDiagnostic();
    }
    return result; // may return { -1, 1, 2, 3 }
  }

  // Instantiate the DOM parser.
  static const XMLCh gLS[] = { chLatin_L, chLatin_S, chNull };
  DOMImplementation *impl = DOMImplementationRegistry::getDOMImplementation(gLS);

  xercesc::DOMBuilder *domParser = ((DOMImplementationLS*)impl)->createDOMBuilder(DOMImplementationLS::MODE_SYNCHRONOUS, 0);
  domParser->setFeature(XMLUni::fgDOMNamespaces, true);
  domParser->setFeature(XMLUni::fgXercesSchema, true);
  domParser->setFeature(XMLUni::fgDOMValidation, true);
  domParser->setFeature(XMLUni::fgDOMValidateIfSchema, true);
  domParser->setFeature(XMLUni::fgXercesSchemaFullChecking, true);
  domParser->setErrorHandler(new DOMErrorReporter());

  // This is a big fat hack.
  // The Xerces DOMBuilder owns the DOMDocument after parsing.  Until a
  // good way to make a copy of the DOMDocument is found, we're going to
  // use a DOMParser per DOMDocument.  There may already be a way to 
  // copy a DOMDocument, and we just haven't seen it... if so then
  // shame on us.
  domParsers.push_back(domParser);

  try
  {
    // reset document pool
    domParser->resetDocumentPool();

	VXIcharToXMLCh membufURL(uri);
    MemBufInputSource membuf(buffer, cbBuffer, membufURL.c_str(), false);
    Wrapper4InputSource domis( &membuf, false );

    *doc = domParser->parse(domis);
    result = 0;
  }
  catch (const XMLException& exception)
  {
    *doc = NULL;
    
    XMLChToVXIchar message(exception.getMessage());
    log.StartDiagnostic(0) << L"DocumentParser::FetchXML - XML parsing "
      L"error from DOM: " << message;
    log.EndDiagnostic();
    log.LogError(999, SimpleLogger::MESSAGE, L"unable to load XML");
    result = 4;
  }
  catch (const DOMException& exception)
  {
    *doc = NULL;

    XMLChToVXIchar message(exception.getMessage());
    log.StartDiagnostic(0) << L"DocumentParser::FetchXML - Parse error: "
                           << message;
    log.EndDiagnostic();
    log.LogError(999, SimpleLogger::MESSAGE, L"unable to load XML");
    result = 4;
  }
  catch( VXIDOMException &exception )
  {
    *doc = NULL;
    log.StartDiagnostic(0) << L"DocumentParser::FetchXML - Parse error: "
      << L"URI=" << exception.getURI()
      << L", line=" << exception.getLine()
      << L", col=" << exception.getColumn()
      << L", Message=" << exception.getMessage();
    log.EndDiagnostic();
    result = 4;
  }
  catch (...)
  {
    *doc = NULL;

    log.StartDiagnostic(0) << L"DocumentParser::FetchXML - Unknown Parse error";
    log.EndDiagnostic();
    log.LogError(999, SimpleLogger::MESSAGE, L"unable to load XML");
    result = 4;
  }

  DocumentParser::ReleaseBuffer(buffer);

  return result;
}

void DocumentParser::XMLDecl(
  const XMLCh* const    versionStr,
  const XMLCh* const    encodingStr,
  const XMLCh* const    standaloneStr,
  const XMLCh* const    autoEncodingStr )
{
  XMLChToVXIchar enc(autoEncodingStr);
  encoding = enc.c_str();
}
