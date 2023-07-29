/**
* @file    Element.h
* @ingroup HtmlBuilder
* @brief   Definitions of an Element in the HTML Document Object Model, and various specialized Element types.
*
* Copyright (c) 2017-2021 Sebastien Rombauts (sebastien.rombauts@gmail.com)
*
* Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
* or copy at http://opensource.org/licenses/MIT)
*/
#pragma once

#include <sstream>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
#include <utility>

/// A simple C++ HTML Generator library.
namespace HTML {

// Note: to configure indentation & minification, define this at compile time before including HTML headers.
#ifndef HTML_INDENTATION
#define HTML_INDENTATION 2
#endif
#ifndef HTML_ENDLINE
#define HTML_ENDLINE "\n"
#endif

/// Convert a boolean to string like std::boolalpha in a std::ostream
constexpr const char* to_string(bool aBool) {
   return aBool ? "true" : "false";
}

/**
* @brief Definitions of an Element in the HTML Document Object Model, and various specialized Element types.
*
* An Element represents any HTML node in the Document Object Model.
*/
class Element {
 public:
   explicit Element(const char* apName, const char* apContent = nullptr) :
                                                                           mName(apName), mContent(apContent ? apContent : "") {}
   Element(const char* apName, std::string&& aContent) :
                                                         mName(apName), mContent(aContent) {}
   Element(const char* apName, const std::string& aContent) :
                                                              mName(apName), mContent(aContent) {}

   Element&& addAttribute(const char* apName, const char* apValue) {
       if (apName && apValue) {
           mAttributes.push_back({ apName, apValue });
       }
       return std::move(*this);
   }
   Element&& addAttribute(const char* apName, const std::string& aValue) {
       mAttributes.push_back({apName, aValue});
       return std::move(*this);
   }
   Element&& addOrAppendAttribute(const char* apName, const char* apValue) {
       if (apName && apValue) {
           for (auto& attribute : mAttributes) {
               if (attribute.Name == apName) {
                   attribute.Value += " ";
                   attribute.Value += apValue;
                   return std::move(*this);
               }
           }
       }
       return addAttribute(apName, apValue);
   }
   Element&& addOrAppendAttribute(const char* apName, const std::string& aValue) {
       for (auto& attribute : mAttributes) {
           if (attribute.Name == apName) {
               attribute.Value += " ";
               attribute.Value += aValue;
               return std::move(*this);
           }
       }
       return addAttribute(apName, aValue);
   }
   Element&& addAttribute(const char* apName, const unsigned int aValue) {
       mAttributes.push_back({apName, std::to_string(aValue)});
       return std::move(*this);
   }
   Element&& operator<<(Element&& aElement) {
       mChildren.push_back(std::move(aElement));
       return std::move(*this);
   }
   Element&& operator<<(const char* apContent);
   Element&& operator<<(std::string&& aContent);
   Element&& operator<<(const std::string& aContent);

   friend std::ostream& operator<<(std::ostream& aStream, const Element& aElement);
   std::string toString() const {
       std::ostringstream stream;
       stream << *this;
       return stream.str();
   }

   Element&& id(const char* apValue) {
       return addAttribute("id", apValue);
   }
   Element&& id(const std::string& aValue) {
       return addAttribute("id", aValue);
   }

   Element&& cls(const char* apValue) {
       return addOrAppendAttribute("class", apValue);
   }
   Element&& cls(const std::string& aValue) {
       return addOrAppendAttribute("class", aValue);
   }

   Element&& title(const char* apValue) {
       return addAttribute("title", apValue);
   }
   Element&& title(const std::string& aValue) {
       return addAttribute("title", aValue);
   }

   Element&& style(const char* apValue) {
       return addAttribute("style", apValue);
   }
   Element&& style(const std::string& aValue) {
       return addAttribute("style", aValue);
   }

   struct Attribute {
       std::string Name;
       std::string Value;
   };

 protected:
   /// Constructor reserved for the Root \<html\> Element as well as the Empty
   Element();

   std::ostream& toString(std::ostream& aStream, const size_t aIndentation = 0) const {
       toStringOpen(aStream, aIndentation);
       toStringContent(aStream, aIndentation);
       toStringClose(aStream, aIndentation);
       return aStream;
   }

 private:
   void toStringOpen(std::ostream& aStream, const size_t aIndentation) const {
       if (!mName.empty()) {
           std::fill_n(std::ostream_iterator<char>(aStream), aIndentation, ' ');
           aStream << '<' << mName;

           for (const auto& attr : mAttributes) {
               aStream << ' ' << attr.Name;
               if (!attr.Value.empty()) {
                   aStream << "=\"" << attr.Value << "\"";
               }
           }

           if (mContent.empty()) {
               // Note: using children for content is less efficient/breaking the assumption
               if (!mChildren.empty() || mbVoid) {
                   aStream << ">" HTML_ENDLINE;
               } else {
                   aStream << ">";
               }
           } else {
               aStream << '>';
           }
       }
   }
   void toStringContent(std::ostream& aStream, const size_t aIndentation) const {
       if (!mName.empty()) {
           aStream << mContent;
           for (auto& child : mChildren) {
               child.toString(aStream, aIndentation + HTML_INDENTATION);
           }
       } else {
           std::fill_n(std::ostream_iterator<char>(aStream), aIndentation, ' ');
           aStream << mContent << HTML_ENDLINE;
       }
   }
   void toStringClose(std::ostream& aStream, const size_t aIndentation) const {
       if (!mName.empty()) {
           if (!mChildren.empty()) {
               std::fill_n(std::ostream_iterator<char>(aStream), aIndentation, ' ');
           }
           // Note: using children for content is less efficient/breaking the assumption
           if (!mContent.empty() || !mChildren.empty() || !mbVoid) {
               aStream << "</" << mName << ">" HTML_ENDLINE;
           }
       }
   }

 protected:
   std::string mName;
   std::string mContent;
   std::vector<Attribute> mAttributes;
   std::vector<Element> mChildren;

   // Self-closing elements complete list:
   // <br> <hr> <img> <input> <link> <meta> <col>
   // <area> <base> <command> <embed> <keygen> <param> <source> <track> <wbr>
   bool mbVoid = false;
};

inline std::ostream& operator<<(std::ostream& aStream, const Element& aElement) {
   return aElement.toString(aStream);
}

/// Empty Element, useful as a default parameter for instance
class Empty : public Element {
 public:
   Empty() : Element() {}
};

/// Raw content text (unnamed Element) to use as text values between child Elements
class Text : public Element {
 public:
   explicit Text(const char* apContent) : Element("", apContent) {}
   explicit Text(std::string&& aContent) : Element("", aContent) {}
   explicit Text(const std::string& aContent) : Element("", aContent) {}
};

inline Element&& Element::operator<<(const char* apContent) {
   return *this << Text(apContent);
}

inline Element&& Element::operator<<(std::string&& aContent) {
   return *this << Text(std::move(aContent));
}

inline Element&& Element::operator<<(const std::string& aContent) {
   return *this << Text(aContent);
}

/// \<title\> Element required in \<head\>
class Title : public Element {
 public:
   explicit Title(const char* apContent) : Element("title", apContent) {}
   explicit Title(const std::string& aContent) : Element("title", aContent) {}
};

/// \<style\> Element for inline CSS in \<head\>
class Style : public Element {
 public:
   explicit Style(const char* apContent) : Element("style", apContent) {}
   explicit Style(const std::string& aContent) : Element("style", aContent) {}
};

/// \<script\> Element for inline Javascript in \<head\>
class Script : public Element {
 public:
   Script() : Element("script") {}
   explicit Script(const char* apSrc) : Element("script") {
       if (apSrc) {
           addAttribute("src", apSrc);
       }
   }
   explicit Script(const char* apSrc, const char* apContent) : Element("script", apContent) {
       if (apSrc) {
           addAttribute("src", apSrc);
       }
   }
   Script&& integrity(const std::string& aValue) {
       addAttribute("integrity", aValue);
       return std::move(*this);
   }
   Script&& crossorigin(const std::string& aValue) {
       addAttribute("crossorigin", aValue);
       return std::move(*this);
   }
};

/// \<meta\> metadata about the Document in \<head\>
class Meta : public Element {
 public:
   Meta() : Element("meta") {}
   explicit Meta(const char* apCharset) : Element("meta") {
       addAttribute("charset", apCharset);
       mbVoid = true;
   }
   explicit Meta(const char* apName, const char* apContent) : Element("meta") {
       addAttribute("name", apName);
       addAttribute("content", apContent);
       mbVoid = true;
   }
};

/// \<link\> Element to reference external CSS or Javascript files
class Rel : public Element {
 public:
   Rel(const char* apRel, const char* apUrl, const char* apType = nullptr) : Element("link") {
       addAttribute("rel", apRel);
       addAttribute("href", apUrl);
       if (apType) {
           addAttribute("type", apType);
       }
       mbVoid = true;
   }

   Rel&& integrity(const std::string& aValue) {
       addAttribute("integrity", aValue);
       return std::move(*this);
   }
   Rel&& crossorigin(const std::string& aValue) {
       addAttribute("crossorigin", aValue);
       return std::move(*this);
   }
};

/// \<base\> Element in \<head\>
class Base : public Element {
 public:
   Base(const std::string& aContent, const std::string& aUrl, const char* apTarget) : Element("base", aContent) {
       addAttribute("href", aUrl);
       if (apTarget) {
           addAttribute("target", apTarget);
       }
   }
};

/// \<head\> required as the first child Element in every HTML Document
class Head : public Element {
 public:
   Head() : Element("head") {}

   Head&& operator<<(Element&& aElement) = delete;
   Head&& operator<<(Title&& aTitle) {
       mChildren.push_back(std::move(aTitle));
       return std::move(*this);
   }
   Head&& operator<<(Style&& aStyle) {
       mChildren.push_back(std::move(aStyle));
       return std::move(*this);
   }
   Head&& operator<<(Script&& aScript) {
       mChildren.push_back(std::move(aScript));
       return std::move(*this);
   }
   Head&& operator<<(Meta&& aMeta) {
       mChildren.push_back(std::move(aMeta));
       return std::move(*this);
   }
   Head&& operator<<(Rel&& aRel) {
       mChildren.push_back(std::move(aRel));
       return std::move(*this);
   }
   Head&& operator<<(Base&& aBase) {
       mChildren.push_back(std::move(aBase));
       return std::move(*this);
   }
};

/// \<body\> required as the second child Element in every HTML Document
class Body : public Element {
 public:
   Body() : Element("body") {}
};

// Constructor of the Root \<html\> Element
inline Element::Element() : mName("html"), mChildren{Head(), Body()} {
}


/// \<br\> Line break Element
class Break : public Element {
 public:
   Break() : Element("br") {
       mbVoid = true;
   }
};

/// \<th\> Table Header Column Element
class ColHeader : public Element {
 public:
   explicit ColHeader(const char* apContent = nullptr) : Element("th", apContent) {}
   explicit ColHeader(std::string&& aContent) : Element("th", aContent) {}
   explicit ColHeader(const std::string& aContent) : Element("th", aContent) {}

   ColHeader&& operator<<(Element&& aElement) {
       mChildren.push_back(std::move(aElement));
       return std::move(*this);
   }

   ColHeader&& rowSpan(const unsigned int aNbRow) {
       if (0 < aNbRow) {
           addAttribute("rowspan", aNbRow);
       }
       return std::move(*this);
   }
   ColHeader&& colSpan(const unsigned int aNbCol) {
       if (0 < aNbCol) {
           addAttribute("colspan", aNbCol);
       }
       return std::move(*this);
   }
};

/// \<td\> Table Column Element
class Col : public Element {
 public:
   explicit Col(const char* apContent = nullptr) : Element("td", apContent) {}
   explicit Col(std::string&& aContent) : Element("td", aContent) {}
   explicit Col(const std::string& aContent) : Element("td", aContent) {}
   explicit Col(const bool abContent) : Element("td", to_string(abContent)) {}
   explicit Col(const int aContent) : Element("td", std::to_string(aContent)) {}
   explicit Col(const unsigned int aContent) : Element("td", std::to_string(aContent)) {}
   explicit Col(const long long aContent) : Element("td", std::to_string(aContent)) {}
   explicit Col(const unsigned long long aContent) : Element("td", std::to_string(aContent)) {}
   explicit Col(const float aContent) : Element("td", std::to_string(aContent)) {}
   explicit Col(const double aContent) : Element("td", std::to_string(aContent)) {}

   Col&& operator<<(Element&& aElement) {
       mChildren.push_back(std::move(aElement));
       return std::move(*this);
   }

   Col&& rowSpan(const unsigned int aNbRow) {
       if (0 < aNbRow) {
           addAttribute("rowspan", aNbRow);
       }
       return std::move(*this);
   }
   Col&& colSpan(const unsigned int aNbCol) {
       if (0 < aNbCol) {
           addAttribute("colspan", aNbCol);
       }
       return std::move(*this);
   }
   Col&& style(const std::string& aValue) {
       Element::style(aValue);
       return std::move(*this);
   }
};

/// \<tr\> Table Row Element
class Row : public Element {
 public:
   Row() : Element("tr") {}

   Row&& operator<<(Element&& aElement) = delete;
   Row&& operator<<(ColHeader&& aCol) {
       mChildren.push_back(std::move(aCol));
       return std::move(*this);
   }
   Row&& operator<<(Col&& aCol) {
       mChildren.push_back(std::move(aCol));
       return std::move(*this);
   }
   Row&& style(const std::string& aValue) {
       Element::style(aValue);
       return std::move(*this);
   }
};

/// \<caption\> Table Caption Element
class Caption : public Element {
 public:
   explicit Caption(const char* apContent) : Element("caption", apContent) {}
};

/// \<table\> Element
class Table : public Element {
 public:
   Table() : Element("table") {}

   Table&& operator<<(Element&& aElement) = delete;
   Table&& operator<<(Row&& aRow) {
       mChildren.push_back(std::move(aRow));
       return std::move(*this);
   }
   Table&& operator<<(Caption&& aCaption) {
       mChildren.push_back(std::move(aCaption));
       return std::move(*this);
   }
};

/// \<li\> List Item Element to put in List
class ListItem : public Element {
 public:
   ListItem() : Element("li") {}
   explicit ListItem(const char* apContent) : Element("li", apContent) {}
   explicit ListItem(const std::string& aContent) : Element("li", aContent) {}

   ListItem&& operator<<(Element&& aElement) {
       mChildren.push_back(std::move(aElement));
       return std::move(*this);
   }

   ListItem&& cls(const std::string& aValue) {
       addOrAppendAttribute("class", aValue);
       return std::move(*this);
   }
};

/// \<ol\> Ordered List or \<ul\> Unordered List Element to use with ListItem
class List : public Element {
 public:
   explicit List(const bool abOrdered = false) : Element(abOrdered?"ol":"ul") {}
   List(const bool abOrdered, const char* apClass) : Element(abOrdered ?"ol":"ul") {
       cls(apClass);
   }

   List&& operator<<(Element&& aElement) = delete;
   List&& operator<<(ListItem&& aItem) {
       mChildren.push_back(std::move(aItem));
       return std::move(*this);
   }
};

/// \<form\> Element
class Form : public Element {
 public:
   explicit Form(const char* apAction = nullptr, const char* apMethod = nullptr) : Element("form") {
       if (apAction) {
           addAttribute("action", apAction);
       }
       if (apMethod) {
           addAttribute("method", apMethod);
       }
   }
};

/// \<input\> Element to use in Form
class Input : public Element {
 public:
   explicit Input(const char* apType = nullptr, const char* apName = nullptr,
                  const char* apValue = nullptr, const char* apContent = nullptr) : Element("input", apContent) {
       if (apType) {
           addAttribute("type", apType);
       }
       if (apName) {
           addAttribute("name", apName);
       }
       if (apValue) {
           addAttribute("value", apValue);
       }
       mbVoid = true;
   }

   Input&& addAttribute(const char* apName, const std::string& aValue) {
       Element::addAttribute(apName, aValue);
       return std::move(*this);
   }
   Input&& addAttribute(const char* apName, const unsigned int aValue) {
       Element::addAttribute(apName, aValue);
       return std::move(*this);
   }

   Input&& id(const std::string& aValue) {
       return addAttribute("id", aValue);
   }
   Input&& cls(const std::string& aValue) {
       addOrAppendAttribute("class", aValue);
       return std::move(*this);
   }
   Input&& title(const std::string& aValue) {
       return addAttribute("title", aValue);
   }
   Input&& style(const std::string& aValue) {
       return addAttribute("style", aValue);
   }

   Input&& size(const unsigned int aSize) {
       return addAttribute("size", aSize);
   }
   Input&& maxlength(const unsigned int aMaxlength) {
       return addAttribute("maxlength", aMaxlength);
   }
   Input&& placeholder(const std::string& aPlaceholder) {
       return addAttribute("placeholder", aPlaceholder);
   }
   Input&& min(const std::string& aMin) {
       return addAttribute("min", aMin);
   }
   Input&& min(const unsigned int aMin) {
       return addAttribute("min", aMin);
   }
   Input&& max(const std::string& aMax) { // NOLINT(build/include_what_you_use) false positive
       return addAttribute("max", aMax);
   }
   Input&& max(const unsigned int aMax) { // NOLINT(build/include_what_you_use) false positive
       return addAttribute("max", aMax);
   }

   Input&& checked(const bool abChecked = true) {
       if (abChecked) {
           addAttribute("checked", "");
       }
       return std::move(*this);
   }
   Input&& autocomplete() {
       return addAttribute("autocomplete", "");
   }
   Input&& autofocus() {
       return addAttribute("autofocus", "");
   }
   Input&& disabled() {
       return addAttribute("disabled", "");
   }
   Input&& readonly() {
       return addAttribute("readonly", "");
   }
   Input&& required() {
       return addAttribute("required", "");
   }
};

/// \<input\> Radio Element to use in Form
class InputRadio : public Input {
 public:
   explicit InputRadio(const char* apName, const char* apValue = nullptr, const char* apContent = nullptr) :
                                                                                                             Input("radio", apName, apValue, apContent) {
   }
};

/// \<input\> Checkbox Element to use in Form
class InputCheckbox : public Input {
 public:
   explicit InputCheckbox(const char* apName, const char* apValue = nullptr, const char* apContent = nullptr) :
                                                                                                                Input("checkbox", apName, apValue, apContent) {
   }
};

/// \<input\> hidden Element to use in Form
class InputHidden : public Input {
 public:
   explicit InputHidden(const char* apName, const char* apValue = nullptr) :
                                                                             Input("hidden", apName, apValue) {
   }
};

/// \<input\> text Element to use in Form
class InputText : public Input {
 public:
   explicit InputText(const char* apName, const char* apValue = nullptr) :
                                                                           Input("text", apName, apValue) {
   }
};

/// \<textarea\> Element to use in Form
class TextArea : public Element {
 public:
   explicit TextArea(const char* apName, const unsigned int aCols = 0, const unsigned int aRows = 0) :
                                                                                                       Element("textarea") {
       addAttribute("name", apName);
       if (0 < aCols) {
           addAttribute("cols", aCols);
       }
       if (0 < aRows) {
           addAttribute("rows", aRows);
       }
   }
   TextArea&& maxlength(const unsigned int aMaxlength) {
       addAttribute("maxlength", aMaxlength);
       return std::move(*this);
   }
};

/// \<intput\> Number Element to use in Form
class InputNumber : public Input {
 public:
   explicit InputNumber(const char* apName, const char* apValue = nullptr) :
                                                                             Input("number", apName, apValue) {
   }
};

/// \<intput\> Range Element to use in Form
class InputRange : public Input {
 public:
   explicit InputRange(const char* apName, const char* apValue = nullptr) :
                                                                            Input("range", apName, apValue) {
   }
};

/// \<intput\> Date Element to use in Form
class InputDate : public Input {
 public:
   explicit InputDate(const char* apName, const char* apValue = nullptr) :
                                                                           Input("date", apName, apValue) {
   }
};

/// \<intput\> Time Element to use in Form
class InputTime : public Input {
 public:
   explicit InputTime(const char* apName, const char* apValue = nullptr) :
                                                                           Input("time", apName, apValue) {
   }
};

/// \<intput\> E-mail Element to use in Form
class InputEmail : public Input {
 public:
   explicit InputEmail(const char* apName, const char* apValue = nullptr) :
                                                                            Input("email", apName, apValue) {
   }
};

/// \<intput\> URL Element to use in Form
class InputUrl : public Input {
 public:
   explicit InputUrl(const char* apName, const char* apValue = nullptr) :
                                                                          Input("url", apName, apValue) {
   }
};

/// \<intput\> Password Element to use in Form
class InputPassword : public Input {
 public:
   explicit InputPassword(const char* apName) :
                                                Input("password", apName) {
   }
};

/// \<intput\> Submit Button Element to use in Form
class InputSubmit : public Input {
 public:
   explicit InputSubmit(const char* apValue = nullptr, const char* apName = nullptr) :
                                                                                       Input("submit", apName, apValue) {
   }
};

/// \<intput\> Reset Button Element to use in Form
class InputReset : public Input {
 public:
   explicit InputReset(const char* apValue = nullptr) :
                                                        Input("reset", nullptr, apValue) {
   }
};

/// \<intput\> List Element to use in Form with DataList
class InputList : public Input {
 public:
   explicit InputList(const char* apName, const char* apList) : Input(nullptr, apName) {
       addAttribute("list", apList);
   }
};

/// \<datalist\> Element for InputList, to use with Option Elements
class DataList : public Element {
 public:
   explicit DataList(const char* apId) : Element("datalist") {
       addAttribute("id", apId);
   }
};

/// \<select\> Element to use with Option Elements
class Select : public Element {
 public:
   explicit Select(const char* apName) : Element("select") {
       addAttribute("name", apName);
   }
};

/// \<option\> Element for Select and DataList
class Option : public Element {
 public:
   explicit Option(const char* apValue, const char* apContent = nullptr) : Element("option", apContent) {
       addAttribute("value", apValue);
   }

   Option&& selected(const bool abSelected = true) {
       if (abSelected) {
           addAttribute("selected", "");
       }
       return std::move(*this);
   }
};

/// \<h1\> Element
class Header1 : public Element {
 public:
   explicit Header1(const std::string& aContent) : Element("h1", aContent) {}
};

/// \<h2\> Element
class Header2 : public Element {
 public:
   explicit Header2(const std::string& aContent) : Element("h2", aContent) {}
};

/// \<h3\> Element
class Header3 : public Element {
 public:
   explicit Header3(const std::string& aContent) : Element("h3", aContent) {}
};

/// \<b\> bold Element
class Bold : public Element {
 public:
   explicit Bold(const std::string& aContent) : Element("b", aContent) {}
};

/// \<i\> italic Element
class Italic : public Element {
 public:
   explicit Italic(const std::string& aContent) : Element("i", aContent) {}
};

/// \<small\> Element for side-comment text and small print, including copyright and legal text
class Small : public Element {
 public:
   Small() : Element("small") {}
   explicit Small(const char* apContent) : Element("small", apContent) {}
   explicit Small(std::string&& aContent) : Element("small", aContent) {}
   explicit Small(const std::string& aContent) : Element("small", aContent) {}
};

/// \<strong\> Element for important text
class Strong : public Element {
 public:
   Strong() : Element("strong") {}
   explicit Strong(const char* apContent) : Element("strong", apContent) {}
   explicit Strong(std::string&& aContent) : Element("strong", aContent) {}
   explicit Strong(const std::string& aContent) : Element("strong", aContent) {}
};

/// \<p\> paragraph Element
class Paragraph : public Element {
 public:
   explicit Paragraph(const std::string& aContent) : Element("p", aContent) {}
};

/// \<div\> division Element to group elements in a rectangular block.
class Div : public Element {
 public:
   Div() : Element("div") {}
   explicit Div(const char* apClass) : Element("div") {
       cls(apClass);
   }

   Div&& cls(const std::string& aValue) {
       addOrAppendAttribute("class", aValue);
       return std::move(*this);
   }
};

/// \<span\> Element to group inline-elements in a document.
class Span : public Element {
 public:
   explicit Span(const std::string& aContent) : Element("span", aContent) {}
};

/// \<pre\> pre-formatted Element to display text in mono-space font.
class Pre : public Element {
 public:
   explicit Pre(const std::string& aContent) : Element("pre", aContent) {}
};

/// \<a\> Hyper-Link Element
class Link : public Element {
 public:
   Link() : Element("a") {}
   explicit Link(const char* apContent) : Element("a", apContent) {}
   explicit Link(const char* apContent, const char* apUrl = nullptr) : Element("a", apContent) {
       if (apUrl) {
           addAttribute("href", apUrl);
       }
   }
   Link(const std::string& aContent, const std::string& aUrl) : Element("a", aContent) {
       if (!aUrl.empty()) {
           addAttribute("href", aUrl);
       }
   }
   Link&& target(const char* apValue) {
       addAttribute("target", apValue);
       return std::move(*this);
   }
};

/// \<img\> Image Element
class Image : public Element {
 public:
   Image(const std::string& aSrc, const std::string& aAlt, unsigned int aWidth = 0, unsigned int aHeight = 0) :
                                                                                                                Element("img") {
       addAttribute("src", aSrc);
       addAttribute("alt", aAlt);
       if (0 < aWidth) {
           addAttribute("width", aWidth);
       }
       if (0 < aHeight) {
           addAttribute("height", aHeight);
       }
       mbVoid = true;
   }
};

/// \<button\> Button Element
class Button : public Element {
 public:
   Button(const char* apContent, const char* apType = "button") :
                                                                  Element("button", apContent) {
       addAttribute("type", apType);
   }
};

/// \<progress\> Element
class Progress : public Element {
 public:
   Progress(const unsigned int aValue, const unsigned int aMax) : Element("progress") {
       addAttribute("value", aValue);
       addAttribute("max", aMax);
   }
};

/// \<meter\> gauge Element
class Meter : public Element {
 public:
   Meter(const unsigned int aValue, const unsigned int aMin, const unsigned int aMax) : Element("meter") {
       addAttribute("value", aValue);
       addAttribute("min", aMin);
       addAttribute("max", aMax);
   }
};

/// \<mark\> semantic Element
class Mark : public Element {
 public:
   explicit Mark(const std::string& aContent) : Element("mark", aContent) {}
};

/// \<time\> semantic Element
class Time : public Element {
 public:
   explicit Time(const std::string& aContent, const std::string& aDateTime) : Element("time", aContent) {
       addAttribute("datetime", aDateTime);
   }
};

/// \<header\> semantic Element
class Header : public Element {
 public:
   Header() : Element("header") {}
};

/// \<footer\> semantic Element
class Footer : public Element {
 public:
   Footer() : Element("footer") {}
};

/// \<section\> semantic Element
class Section : public Element {
 public:
   Section() : Element("section") {}
};

/// \<article\> semantic Element
class Article : public Element {
 public:
   Article() : Element("article") {}
};

/// \<nav\> semantic Element
class Nav : public Element {
 public:
   Nav() : Element("nav") {}
   explicit Nav(const char* apClass) : Element("nav") {
       cls(apClass);
   }
};

/// \<aside\> semantic Element
class Aside : public Element {
 public:
   Aside() : Element("aside") {}
};

/// \<main\> semantic Element
class Main : public Element {
 public:
   Main() : Element("main") {}
};

/// \<figure\> semantic Element
class Figure : public Element {
 public:
   Figure() : Element("figure") {}
};

/// \<figcaption\> semantic Element to use with Figure
class FigCaption : public Element {
 public:
   explicit FigCaption(const std::string& aContent) : Element("figcaption", aContent) {}
};

/** @brief \<details\> semantic Element containing detailed information to use with Summary.
*
* @verbatim
<details>
 <summary>Copyright 2017-2021.</summary>
 <p>By Sebastien Rombauts.</p>
 <p>sebastien.rombauts@gmail.com.</p>
</details> @endverbatim
*/
class Details : public Element {
 public:
   explicit Details(const char* apOpen = nullptr) : Element("details") {
       if (apOpen) {
           addAttribute("open", apOpen);
       }
   }
};

/// \<summary\> semantic Element to use inside a Details section to specify a visible heading
class Summary : public Element {
 public:
   explicit Summary(const std::string& aContent) : Element("summary", aContent) {}
};


} // namespace HTML
