/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */

// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  docs: [
    'Readme',
    'introduction',
    {'Crafting Interpeters': ['lox-language','map-of-the-territory', 'scanning', 'representing-code'].map(i => 'book/'+i)},
    {'Functional Programming': ['parser-combinators'].map(i => 'fp/'+i)},
    'resources'
  ]
};

module.exports = sidebars;
