/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  async redirects() {
    return [
      {
        source: '/docs/getting_started',
        destination: '/docs/getting-started',
        permanent: true,
      },
    ]
  },
};

module.exports = nextConfig;
