# Contribution guidelines for `home-assistant-client`

## Release process

- Ensure that the package version in `package.yaml` is as expected.
- Ensure that the `CHANGELOG.md` file contains the expected changes and release date.
- Remove the "Unreleased" section from the changelog.
- Ensure that CI checks pass for all of this on some branch.
- After the CI checks pass, tag the commit, and push it with (for example) `git push origin refs/tags/v0.1`
- Create a draft release for the release tag
- Check that the "Hackage" CI passes and that the corresponding workflow log does not contain any problems.
- Publish the release.
