// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Grox',
  tagline: 'A programming language written in Scala 3',
  url: 'https://grokking-vietnam.github.io/',
  baseUrl: '/grox/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'grokking-vietnam', // Usually your GitHub org/user name.
  projectName: 'grox', // Usually your repo name.

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: '../grox-docs/target/mdoc',
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          editUrl: 'https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/',
        },
        blog: false,
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'Grox',
        logo: {
          alt: 'Grox Language Logo',
          src: 'https://avatars.githubusercontent.com/u/9639466?s=45&v=4',
        },
        items: [
          {
            label: 'Learn',
            to: '/docs'
          },
          {
            href: 'https://github.com/grokking-vietnam/grox',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Learn',
            items: [
              {
                label: 'The Grox Programming Language',
                to: '/docs/introduction',
              },
              {
                label: 'Resources',
                to: '/docs/resources',
              },
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Grokking Vietnam',
                href: 'https://www.grokking.org/',
              },
              {
                label: 'Facebook',
                href: 'https://www.facebook.com/grokking.vietnam/',
              },
              {
                label: 'Youtube',
                href: 'https://www.youtube.com/channel/UCH2gAK9r_7EbvyVt0z5VibQ/featured',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'GitHub',
                href: 'https://github.com/grokking-vietnam/grox',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Grokking Vietnam. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme
      },
    }),
};

module.exports = config;
