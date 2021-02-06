/* eslint-disable no-undef */
module.exports = {
   preset: 'ts-jest',
   testEnvironment: 'node',
   modulePathIgnorePatterns: ['dist'],
   setupFilesAfterEnv: ['./jest.setup.js'],
   collectCoverage: true,
}
