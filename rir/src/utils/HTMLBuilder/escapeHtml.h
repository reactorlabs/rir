//
// Created by Jakob Hain on 7/29/23.
//

#pragma once

#include <string>

static std::string escapeHtml(const std::string& s) {
    std::string res;
    res.reserve(s.size());
    for (auto c : s) {
        switch (c) {
        case '&':
            res += "&amp;";
            break;
        case '\"':
            res += "&quot;";
            break;
        case '\'':
            res += "&apos;";
            break;
        case '<':
            res += "&lt;";
            break;
        case '>':
            res += "&gt;";
            break;
        default:
            res += c;
        }
    }
    return res;
}