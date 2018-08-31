# FOURJS_START_COPYRIGHT(U,2014)
# Property of Four Js*
# (c) Copyright Four Js 2014, 2015. All Rights Reserved.
# * Trademark of Four Js Development Tools Europe Ltd
#   in the United States and elsewhere
# 
# Four Js and its suppliers do not warrant or guarantee that these samples
# are accurate and suitable for your purposes. Their inclusion is purely for
# information purposes only.
# FOURJS_END_COPYRIGHT
#

#uncomment this to enable debugging
#&define DEBUG

IMPORT XML

CONSTANT LI_IDENTATION_METHOD = 1 # 1=cascade, 2=CSS margin, 3=CSS padding
CONSTANT LI_INDENTATION_VALUE_IN_PX = 20

# The three objects we need to handle
PRIVATE DEFINE cleanDocHTML, richTextDocGDC xml.DomDocument
PRIVATE DEFINE baseFontWeight, baseFontSize, fontSizeGap, fontWeightGap INTEGER

MAIN

  DEFINE html5Node xml.DomDocument
  
  IF NUM_ARGS() == 2 THEN
    LET html5Node = rtm_MigrateRichTextFile(ARG_VAL(1))
    CALL html5Node.save(ARG_VAL(2))
  ELSE
    IF NUM_ARGS() == 1 THEN
      LET html5Node = rtm_MigrateRichTextFile(ARG_VAL(1))
      DISPLAY html5Node.saveToString()
    ELSE
      DISPLAY "ERROR : Invalid argument number"
      DISPLAY "USAGE : fglrun RichTextMigrationHelper richtextCode.html [h5Result.html]"
      EXIT PROGRAM(-1)
    END IF
  END IF
  
END MAIN

PUBLIC FUNCTION rtm_MigrateRichTextFile(fileName)
  DEFINE fileName STRING
  CALL rtm_LoadRichtextToClean(fileName)
  CALL rtm_MigrateRichText()

  RETURN cleanDocHTML;
END FUNCTION


PRIVATE FUNCTION rtm_MigrateRichText()
  DEFINE bodys xml.DomNodeList
  DEFINE GDCbody xml.DomNode
  DEFINE bodyFontSize, bodyFontWeight INTEGER

  -- set the W3C standard
  LET baseFontSize = 12
  LET baseFontWeight = 400
  LET fontSizeGap = 2
  LET fontWeightGap = 100

  CALL rtm_InitializeResultingDoc()

  LET bodys = richTextDocGDC.getElementsByTagName("body")
  IF bodys.getCount() == 0 THEN
    CALL rtm_DisplayError("Could not get body element in richtext code")
  END IF

    # there should be only one body
  LET GDCbody = bodys.getItem(1)

  LET bodyFontSize = rtm_DetectFontSize(GDCbody)
  LET bodyFontWeight = rtm_DetectFontWeight(GDCbody)
  IF rtm_Abs(bodyFontSize - baseFontSize) > fontSizeGap THEN
    -- the detected font size is different from the standard of more than 2pt
    LET baseFontSize = bodyFontSize
  END IF
  IF rtm_Abs(bodyFontWeight -baseFontWeight) > fontWeightGap THEN
    -- the detected font size is different from the standard of more than 100
    LET baseFontWeight = bodyFontWeight
  END IF

  CALL rtm_MigrateNode(GDCbody, cleanDocHTML.getDocumentElement())
END FUNCTION


PRIVATE FUNCTION rtm_MigrateNode(GDCnode, HTMLnode)
  DEFINE GDCnode, HTMLnode, GDCnext, HTMLnext, useless xml.DomNode
  DEFINE COUNT  INTEGER

  CASE GDCnode.getLocalName()
    WHEN "p"
      LET HTMLnext = rtm_HandlePNode(GDCnode, HTMLnode)
    WHEN "span"
      LET HTMLnext = rtm_HandleSpanNode(GDCnode, HTMLnode)
    WHEN "ol"
      LET HTMLnext = rtm_HandleOlUlNode(GDCnode, HTMLnode)
    WHEN "ul"
      LET HTMLnext = rtm_HandleOlUlNode(GDCnode, HTMLnode)
    WHEN "li"
      LET HTMLnext = rtm_HandleLiNode(GDCnode, HTMLnode)
    OTHERWISE
      IF GDCnode.getAttribute("style") IS NOT NULL THEN
        LET HTMLnext = rtm_HandleNodeWithStyleAttr(GDCnode, HTMLnode)
      ELSE
        LET HTMLnext = rtm_HandleRegularNode(GDCnode, HTMLnode)
      END IF
  END CASE

  FOR count = 1 TO GDCnode.getChildrenCount() STEP 1
    LET GDCnext = GDCnode.getChildNodeItem(COUNT)
    IF GDCnext.getNodeType() == "ELEMENT_NODE" THEN
      CALL rtm_MigrateNode(GDCnext, HTMLnext)
    ELSE 
      IF GDCnext.getNodeType() == "TEXT_NODE" THEN
        IF NOT rtm_CheckIgnorableWhiteSpace(GDCNext.getNodeValue()) THEN
          LET useless = rtm_HandleRegularNode(GDCnext, HTMLnext)
        END IF 
      END IF
    END IF
  END FOR
END FUNCTION

PRIVATE FUNCTION rtm_CheckIgnorableWhiteSpace(str)
  DEFINE    str STRING
  DEFINE    ind INTEGER
  DEFINE    c   CHAR
  FOR ind=1 TO str.getLength()
    LET c=str.getCharAt(ind)
    IF c!='\n' AND c!='\r' AND c!='\t' AND c!=' ' THEN
      RETURN FALSE
    END IF
  END FOR
  RETURN TRUE
END FUNCTION

PRIVATE FUNCTION rtm_HandleRegularNode(GDCnode, HTMLnode)
  DEFINE GDCnode, HTMLnode, HTMLnewNode xml.DomNode

  TRY
    LET HTMLnewNode = cleanDocHTML.importNode(GDCnode, FALSE)
  CATCH
    CALL rtm_DisplayError("Could not import the GDC node")
  END TRY
  
  IF HTMLnewNode IS NULL THEN
    CALL rtm_DisplayError("Could not import the GDC node, NULL returned")
  END IF

  CALL HTMLnode.appendChild(HTMLnewNode)

  RETURN HTMLnewNode
END FUNCTION


PRIVATE FUNCTION rtm_HandleNodeWithStyleAttr(GDCnode, HTMLnode)
  DEFINE GDCnode, HTMLnode, HTMLnewNode xml.DomNode
  DEFINE styleVal STRING

  LET HTMLnewNode = rtm_HandleRegularNode(GDCnode, HTMLnode)
  LET styleVal = rtm_GetAttributeValue("style", GDCnode)

  IF styleVal IS NOT NULL THEN
    LET styleVal = rtm_BuildNewPaddingStyleAttrValue(styleVal)
  END IF

  IF styleVal IS NOT NULL THEN
    CALL HTMLnewNode.setAttribute("style", styleVal)
  END IF

  CALL rtm_RemoveQTStyleAttr(HTMLnewNode)

  RETURN HTMLnewNode
END FUNCTION

PRIVATE FUNCTION rtm_GetQtBlockIndentValue(GDCNode)
    DEFINE GDCnode xml.DomNode,
         styleVal STRING,
         indexQtIndent, indexQtIndentEnd, qtIndentVal INTEGER

    LET styleVal = rtm_GetAttributeValue("style", GDCnode)
    LET qtIndentVal = 0
    IF styleVal IS NOT NULL THEN
        LET indexQtIndent = styleVal.getIndexOf("-qt-block-indent:", 1)
        IF indexQtIndent > 0 THEN
            LET indexQtIndentEnd = styleVal.getIndexOf(";", indexQtIndent)
            LET qtIndentVal = styleVal.substring(indexQtIndent+17, indexQtIndentEnd-1)
        END IF
    END IF

    RETURN qtIndentVal
END FUNCTION


#search the fragment for the most recently added li.
PRIVATE FUNCTION rtm_GetMostRecentlyAddedLi(HTMLnode)
    DEFINE HTMLnode, currentNode, result xml.DomNode

#Implementation: Return the first li element encountered in a reverse post order traverse of the fragment
    LET currentNode = HTMLnode.getLastChild()
    WHILE currentNode IS NOT NULL
        LET result = rtm_GetMostRecentlyAddedLi(currentNode)
        IF result IS NOT NULL THEN
            RETURN result
        END IF
        LET currentNode = currentNode.getPreviousSiblingElement()
    END WHILE

    IF HTMLnode.getLocalName() == "li" THEN
        RETURN HTMLnode
    ELSE
        RETURN NULL
    END IF
END FUNCTION


PRIVATE FUNCTION rtm_HandleLiNode(GDCnode, HTMLnode)
    DEFINE GDCnode, HTMLnode xml.DomNode
    CASE LI_IDENTATION_METHOD
    WHEN 1
        RETURN rtm_HandleLiNodeCascade(GDCnode, HTMLnode)
    WHEN 2
        RETURN rtm_HandleLiNodeViaStyle(GDCnode, HTMLnode, "margin-left")
    OTHERWISE
        RETURN rtm_HandleLiNodeViaStyle(GDCnode, HTMLnode, "padding-left")
    END CASE
END FUNCTION


PRIVATE FUNCTION rtm_HandleLiNodeViaStyle(GDCnode, HTMLnode, styleName)
    DEFINE GDCnode, HTMLnode,HTMLnewNode xml.DomNode, qtIndentVal INTEGER,
           styleName, alignVal, styleVal STRING

    LET qtIndentVal = rtm_GetQtBlockIndentValue(GDCnode)
    LET alignVal = rtm_GetAttributeValue("align", GDCnode)

    IF alignVal IS NOT NULL THEN
      LET styleVal = "text-align:", styleVal,";"
    END IF
    IF styleVal IS NULL THEN
        LET styleVal = styleName,":", (qtIndentVal * LI_INDENTATION_VALUE_IN_PX), "px;"
    ELSE
        LET styleVal = styleVal, styleName, ":", (qtIndentVal * LI_INDENTATION_VALUE_IN_PX), "px;"
    END IF

    LET HTMLnewNode = rtm_CreateLi(styleVal)
    CALL HTMLNode.appendChild(HTMLnewNode)

    RETURN HTMLnewNode
END FUNCTION


PRIVATE FUNCTION rtm_HandleLiNodeCascade(GDCnode, HTMLnode)
    DEFINE GDCnode, HTMLnode, HTMLnewNode,parentOlUl,previousLi,previouslyAddedLi,newNode xml.domNode,
           qtIndentVal, qtIndentValPreviousLi, i INTEGER,
           alignVal STRING

    LET qtIndentVal = rtm_GetQtBlockIndentValue(GDCnode)
    LET alignVal = rtm_GetAttributeValue("align", GDCnode)

    IF alignVal IS NOT NULL THEN
      LET alignVal = "text-align:", alignVal,";"
    END IF

    -- We are in a list
    -- at least, we have one li node
    LET HTMLnewNode = rtm_CreateLi(alignVal)
    IF qtIndentVal == 0 THEN
        LET parentOlUl = HTMLnode
    ELSE
        LET previousLi = GDCNode.getPreviousSibling()
#skip character data if any
        WHILE previousLi IS NOT NULL
            IF previousLi.getLocalName() == "li" THEN
                 EXIT WHILE
            END IF
            LET previousLi = previousLi.getPreviousSibling()
        END WHILE
        IF previousLi IS NULL THEN
#If we get here then this means that a list starts with a non 0 list item.
            LET qtIndentValPreviousLi = 0
            LET parentOlUl = rtm_CreateLiOlList(HTMLnode, qtIndentVal, HTMLNode.getLocalName(), alignVal)
        ELSE
            LET qtIndentValPreviousLi = rtm_GetQtBlockIndentValue(previousLi)
            LET previouslyAddedLi = rtm_GetMostRecentlyAddedLi(HTMLnode)
            IF previouslyAddedLi IS NULL THEN
#assert false. We should't get here since we add a li to HTML for every li in GDC
                CALL rtm_DisplayError("Internal error 2")
            END IF
            IF qtIndentVal <= qtIndentValPreviousLi THEN
                LET parentOlUl = previouslyAddedLi.getParentNode()
                FOR i=qtIndentVal + 1 TO qtIndentValPreviousLi
#get parent li
                    LET parentOlUl = parentOlUl.getParentNode()
#get parent ol/ul
                    LET parentOlUl = parentOlUl.getParentNode()
                END FOR
            ELSE #qtIndentVal>qtIndentValPreviousLi
                LET newNode = cleanDocHTML.createElement(HTMLnode.getLocalName())
                CALL previouslyAddedLi.appendChild(newNode)
                LET parentOlUl = newNode
                LET parentOlUl = rtm_CreateLiOlList(newNode, qtIndentVal - qtIndentValPreviousLi - 1, HTMLNode.getLocalName(), alignVal)
            END IF
        END IF
    END IF

&ifdef DEBUG
DISPLAY "DEBUG rtm_HandleLiNode: appending " || rtm_GetInfo(HTMLnewNode) || " to " || rtm_GetInfo(parentOlUl) || " resulting doc:" || rtm_ToString(cleanDocHTML.getDocumentElement())
&endif

    CALL parentOlUl.appendChild(HTMLnewNode)

&ifdef DEBUG
    CALL rtm_AssertIndentation(qtIndentVal, HTMLnewNode, HTMLnode)
&endif

  RETURN HTMLnewNode
END FUNCTION


#creates a list of nested li/ol and returns the innermost ol
FUNCTION rtm_CreateLiOlList(parent,amount,olName,alignVal)
    DEFINE parent, newNode xml.DomNode,
           amount INTEGER,
           olName, alignVal STRING

    WHILE amount > 0
        LET newNode = rtm_CreateLi(alignVal)
        CALL parent.appendChild(newNode)
        LET parent = newNode
        LET newNode = cleanDocHTML.createElement(olName)
        CALL parent.appendChild(newNode)
        LET parent = newNode
        LET amount = amount-1
    END WHILE

    RETURN parent
END FUNCTION


PRIVATE FUNCTION rtm_HandleOlUlNode(GDCnode, HTMLnode)
    DEFINE GDCnode, HTMLnode, HTMLnewNode xml.DomNode

    LET HTMLnewNode = rtm_HandleRegularNode(GDCnode, HTMLnode)

    CALL rtm_RemoveQTStyleAttr(HTMLnewNode)

    RETURN HTMLnewNode
END FUNCTION

PRIVATE FUNCTION rtm_HandleSpanNode(GDCnode, HTMLnode)
    DEFINE GDCnode, HTMLnode, HTMLnewNode, HTMLnewNode2, HTMLStyleNode xml.DomNode
    DEFINE fontSize, fontWeight, textDecoIndex INTEGER
    DEFINE styleVal, newStyleVal, newNodeStyleVal STRING

    LET HTMLnewNode = rtm_HandleRegularNode(GDCnode, HTMLnode)
    LET HTMLStyleNode = HTMLNewNode # Keep specific node holding style for later

    LET fontSize = rtm_DetectFontSize(GDCnode)
    LET fontWeight = rtm_DetectFontWeight(GDCnode)
    LET styleVal = rtm_GetAttributeValue("style", GDCnode)

    IF fontSize > baseFontSize OR fontWeight > baseFontWeight THEN
        LET HTMLnewNode2 = cleanDocHTML.createElement("strong")

        IF HTMLnewNode2 IS NULL THEN
            CALL rtm_DisplayError("Could not create strong node, NULL returned")
        END IF

        CALL HTMLnewNode.appendChild(HTMLnewNode2)
        LET HTMLnewNode = HTMLnewNode2
        
    ELSE
        IF fontSize < baseFontSize THEN
            LET HTMLnewNode2 = cleanDocHTML.createElement("small")

            IF HTMLnewNode2 IS NULL THEN
                CALL rtm_DisplayError("Could not create small node, NULL returned")
            END IF

            CALL HTMLnewNode.appendChild(HTMLnewNode2)
            LET HTMLnewNode = HTMLnewNode2
        END IF
        -- if font weight or size does not change we manage the node as is
    END IF

    IF styleVal IS NOT NULL THEN
        IF styleVal.getIndexOf("font-style:italic", 1) > 0 THEN
            LET HTMLnewNode2 = cleanDocHTML.createElement("em")

            IF HTMLnewNode2 IS NULL THEN
                CALL rtm_DisplayError("Could not create em node, NULL returned")
            END IF

            CALL HTMLnewNode.appendChild(HTMLnewNode2)
            LET HTMLnewNode = HTMLnewNode2
        END IF

        IF styleVal.getIndexOf("text-decoration", 1) > 0 THEN
            LET textDecoIndex = styleVal.getIndexOf("text-decoration", 1)
            LET newStyleVal = styleVal.substring(textDecoIndex, styleVal.getIndexOf(";", textDecoIndex))

            LET newNodeStyleVal = rtm_GetAttributeValue("style", HTMLnewNode)
            IF newNodeStyleVal IS NOT NULL THEN
                LET newNodeStyleVal = newNodeStyleVal, newStyleVal
            ELSE
                LET newNodeStyleVal = newStyleVal
            END IF

            CALL HTMLStyleNode.setAttribute("style", newNodeStyleVal)

        END IF
    END IF

    RETURN HTMLnewNode
END FUNCTION


PRIVATE FUNCTION rtm_HandlePNode(GDCnode, HTMLnode)
  DEFINE GDCnode, HTMLnode, HTMLnewNode xml.DomNode
  DEFINE styleVal, alignVal STRING

  LET HTMLnewNode = rtm_HandleRegularNode(GDCnode, HTMLnode)
  LET alignVal = rtm_GetAttributeValue("align", HTMLnewNode)
  LET styleVal = rtm_GetAttributeValue("style", HTMLnewNode)

  IF alignVal IS NOT NULL THEN
    CALL HTMLnewNode.removeAttribute("align")
  END IF

  IF styleVal IS NOT NULL THEN
    LET styleVal = rtm_BuildNewPaddingStyleAttrValue(styleVal)
  END IF

  IF styleVal IS NOT NULL AND alignVal IS NOT NULL THEN
    LET styleVal = styleVal, alignVal
  ELSE
    IF styleVal IS NULL THEN
      LET styleVal = alignVal
    END IF
  END IF

  IF styleVal.getLength() > 0 THEN
    CALL HTMLnewNode.setAttribute("style", styleVal)
  END IF

  CALL rtm_RemoveQTStyleAttr(HTMLnewNode)

  RETURN HTMLnewNode

END FUNCTION

PRIVATE FUNCTION rtm_CreateLi(styleAlignVal)
  DEFINE HTMLnewNode xml.DomNode
  DEFINE styleAlignVal STRING

  LET HTMLnewNode = cleanDocHTML.createElement("li")

  IF HTMLnewNode IS NULL THEN
    CALL rtm_DisplayError("Could not create li node, NULL returned")
  END IF

  IF styleAlignVal IS NOT NULL AND styleAlignVal.getLength() > 0 THEN
    CALL HTMLnewNode.setAttribute("style", styleAlignVal)
  END IF

  RETURN HTMLnewNode
END FUNCTION

PRIVATE FUNCTION rtm_BuildNewPaddingStyleAttrValue(attrVal)
  DEFINE attrVal, attrNewVal, paddingValStr STRING
  DEFINE indexQtIndent, indexQtIndentEnd, qtIndentVal, paddingVal INTEGER

  LET attrNewVal = NULL
  IF attrVal IS NOT NULL THEN
    LET indexQtIndent = attrVal.getIndexOf("-qt-block-indent:", 1)
    IF indexQtIndent > 0 THEN
      LET indexQtIndentEnd = attrVal.getINdexOf(";", indexQtIndent)
      LET qtIndentVal = attrVal.substring(indexQtIndent+17, indexQtIndentEnd - 1)
      IF qtIndentVal > 0 THEN
        LET paddingVal = qtIndentVal * 30
        LET paddingValStr = paddingVal

        LET attrNewVal = "padding-left:", paddingValStr.trim(), "px;"
      END IF
    END IF
  END IF

  RETURN attrNewVal
END FUNCTION


PRIVATE FUNCTION rtm_RemoveQTStyleAttr(HTMLnewNode)
  DEFINE HTMLnewNode xml.DomNode
  DEFINE attrVal STRING
  DEFINE newVal STRING
  DEFINE token  STRING
  DEFINE tokenizer  base.StringTokenizer
  LET attrVal = rtm_GetAttributeValue("style", HTMLnewNode)
  IF attrVal IS NOT NULL THEN
      IF attrVal.getIndexOf("-qt-", 1) != 0 THEN
          # Remove all qt styles and let others untouched
          LET tokenizer = base.StringTokenizer.Create(attrVal,";")
          WHILE tokenizer.hasMoreTokens()
            LET token = tokenizer.nextToken()
            IF token.getIndexOf("-qt-",1) == 0 THEN
              IF newVal IS NULL THEN
                LET newVal = token
              ELSE
                LET newVal = newVal||"; "||token
              END IF
            END IF
          END WHILE
          IF newVal IS NOT NULL THEN
            CALL HTMLnewNode.setAttribute("style",newVal)
          ELSE
            CALL HTMLnewNode.removeAttribute("style")
          END IF
      END IF
  END IF

END FUNCTION


PRIVATE FUNCTION rtm_GetAttributeValue(name,node)
    DEFINE name STRING,
           node xml.DomNode

    RETURN node.getAttribute(name)
END FUNCTION


PRIVATE FUNCTION rtm_DetectFontSize(node)
  DEFINE node xml.DomNode
  DEFINE sizeAttr STRING
  DEFINE fontSizeIndex, fontSize INTEGER

  LET fontSize = 12
  LET sizeAttr = rtm_GetAttributeValue("style", node)

  IF sizeAttr IS NOT NULL THEN
    LET fontSizeIndex = sizeAttr.getIndexOf("font-size:", 1)
    IF fontSizeIndex > 0 THEN
      LET fontSize = sizeAttr.substring(fontSizeIndex + 10, sizeAttr.getIndexOf("pt;", fontSizeIndex) - 1)
    END IF
  END IF

  RETURN fontSize
END FUNCTION


PRIVATE FUNCTION rtm_DetectFontWeight(node)
  DEFINE node xml.DomNode
  DEFINE weightAttr STRING
  DEFINE fontWeightIndex INTEGER
  DEFINE fontWeight INTEGER

  LET fontWeight = 400
  LET weightAttr = rtm_GetAttributeValue("style", node)

  IF weightAttr IS NOT NULL THEN
    LET fontWeightIndex = weightAttr.getIndexOf("font-weight:", 1)
    IF fontWeightIndex > 0 THEN
      LET fontWeight = weightAttr.substring(fontWeightIndex + 12, weightAttr.getIndexOf(";", fontWeightIndex) - 1)
    END IF
  END IF

  RETURN fontWeight
END FUNCTION


PRIVATE FUNCTION rtm_InitializeResultingDoc()

  # Create the document structure <HTML></HTML>
  LET cleanDocHTML = xml.DomDocument.createDocument("html")
  # to prevent xml declaration to be added to the document
  CALL cleanDocHTML.setFeature("enable-html-compliancy", TRUE)
  CALL cleanDocHTML.setFeature("format-pretty-print",TRUE)

END FUNCTION


# Complex richText document are not HTML valid
PRIVATE FUNCTION rtm_LoadRichtextToClean(toLoad)
  DEFINE toLoad STRING

  TRY
    LET richTextDocGDC = xml.DomDocument.create()
    CALL richTextDocGDC.setFeature("enable-html-compliancy", TRUE)
    CALL richTextDocGDC.setFeature("whitespace-in-element-content", FALSE)
    CALL richTextDocGDC.load(toLoad)
  CATCH
    CALL rtm_DisplayError("Unable to load document")
  END TRY 
  
END FUNCTION

PRIVATE FUNCTION rtm_DisplayError(message)
  DEFINE message STRING

  DISPLAY message
  EXIT PROGRAM -1
END FUNCTION


PRIVATE FUNCTION rtm_Abs(val)
    DEFINE val INTEGER

    IF val < 0 THEN
        RETURN -val
    ELSE
        RETURN val
    END IF
END FUNCTION


&ifdef DEBUG
PRIVATE FUNCTION rtm_AssertIndentation(expectedIndentation,newLi,listRoot)
    DEFINE expectedIndentation,count INTEGER,
           newLi,listRoot, currentNode xml.DomNode

    LET count = 0
    LET currentNode = newLi.getParentNode()
    WHILE currentNode IS NOT NULL
        IF currentNode.getLocalName() == "li" THEN
            LET count = count + 1
        END IF
       LET currentNode = currentNode.getParentNode()
    END WHILE
    IF count != expectedIndentation THEN
         DISPLAY "DEBUG newLi=" || rtm_GetInfo(newLi)
        CALL rtm_DisplayError("Internal error 3: count=" || count || ", expectedIndentation=" || expectedIndentation)
    END IF
END FUNCTION

PRIVATE FUNCTION rtm_ToString(node)
    DEFINE node, currentNode xml.DomNode,
           result STRING

    LET result = node.getLocalName()
    LET currentNode = node.getFirstChild()
    IF currentNode IS NOT NULL THEN
        LET result = result || "{" || rtm_ToString(currentNode)
        LET currentNode = currentNode.getNextSibling()
        WHILE currentNode IS NOT NULL
            LET result=result || "," || rtm_ToString(currentNode)
            LET currentNode = currentNode.getNextSibling()
        END WHILE
        LET result = result || "}"
    END IF

    RETURN result
END FUNCTION


PRIVATE FUNCTION rtm_GetInfo(node)
    DEFINE node xml.DomNode

    IF node.getParentNode() IS NULL THEN
        RETURN "/" || rtm_ToString(node)
    ELSE
        RETURN rtm_GetPath(node.getParentNode()) || "/" || rtm_ToString(node)
    END IF
END FUNCTION


PRIVATE FUNCTION rtm_GetPath(node)
    DEFINE node xml.DomNode,
           result STRING

    IF node.getParentNode() IS NULL THEN
        RETURN node.getLocalName()
    ELSE
        RETURN rtm_GetPath(node.getParentNode()) || "/" || node.getLocalName()
    END IF
END FUNCTION
&endif
