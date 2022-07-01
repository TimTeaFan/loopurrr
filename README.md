
<!-- README.md is generated from README.Rmd. Please edit that file -->

# loopurrr’s Twitter Like Social Badge

<!-- badges: start -->

![GitHub Workflow
Status](https://img.shields.io/github/workflow/status/TimTeaFan/loopurrr/like-banner?label=bot%20status&logo=Github%20Actions&logoColor=white&style=flat-square)
![GH
Actions](https://img.shields.io/static/v1?label=automated%20with&message=GitHub%20Actions&color=2088FF&logo=GitHub%20Actions&style=flat-square&labelColor=52535e)
[<img src="https://img.shields.io/badge/dynamic/json?url=https://raw.githubusercontent.com/TimTeaFan/loopurrr/twitter-like-badge/likes&label=Likes&query=$.data..public_metrics.like_count&style=social&logo=Twitter">](https://twitter.com/timteafan/status/1511034067344572422?s=21)
<!-- badges: end -->

## About

This branch creates the “Twitter Like Social Badge” as displayed above.
The badge shows the number of likes the [tweet that announced the
loopurrr
package](https://twitter.com/timteafan/status/1511034067344572422?s=21&t=WEhGBqj-5dtsFIYCuAtmpg)
has received so far.

This README documents how I built the badge using shields.io and Github
Action. This is not only for my future self (who won’t remember), but
also for anyone who is interested in building a similar badge.

The setup consists of four steps:

1.  sign up for the Twitter API
2.  get the number of likes of a specific Tweet
3.  build a GitHub Actions workflow that gets the Twitter data and saves
    it to a dedicated branch
4.  add an image based on a dynamic shields.io URL to the README

#### 1. Twitter Developer Plattform

This step was pretty straightforward: just sign up
[here](https://developer.twitter.com). Since the number of likes of a
Tweet can be accessed via the API v2, “essential access” is enough - we
do not need to apply for the more restrictive “elevated access”.

#### 2. The Twitter API

Looking at the
[docs](https://developer.twitter.com/en/docs/twitter-api/tweets/likes/introduction)
I thought `GET /2/tweets/:id/liking_users` would be the way to go about
it, but this returns a list of all user names, whereas I only needed the
number of likes. The following was what I was looking for:

`GET /2/tweets?ids=<tweetID>&tweet.fields=public_metrics`

Originally I wanted to use the {rtweet} package, but realizing that I
only need one command to get the data I figured a call to `curl` from
the command line was enough for my needs, especially since we can save
the output with `curl <URL> -o my.file`

#### 3. Github Actions

This was the toughest part. Up until now I just used existing \#RStats
Github Action Workflows like RMD Check or pkgdown. They were easy to set
up, but looking at the YAML I had no idea how to customize them further,
let alone create one from scratch. First, I started with the OS. Since I
use a Mac at home I chose: `runs-on: macOS-latest` to be able to test
single steps in a similar environment I later switched this to
`ubuntu-latest`).

Next, I wanted to run the workflow on a dedicated branch. To do this we
need `uses: actions/checkout@v3` because from version 3 on we can
specify

``` yaml
with:
    ref: mybranch
```

Since I later wanted to commit and push the data, it was necessary to
set-up git first (thanks to [@Lluis_Revilla’s
help](https://twitter.com/lluis_revilla/status/1524805074337288194?s=21&t=WEhGBqj-5dtsFIYCuAtmpg)
on this)

``` yaml
run: |
  git config user.email github-actions@guthub.com
  git config user.name github-actions`
```

Getting the actual data from Twitter is just one call to curl. Here I
wasn’t sure how to access the Bearer Token but looking at other
workflows helped a lot:

``` sh
curl 'https://api.twitter.com/2/tweets?ids=1511034067344572422&tweet.fields=public_metrics&expansions=attachments.media_keys&media.fields=public_metrics' --header 'Authorization: Bearer ${{ secrets.TWITTER_BEARER_TOKEN }}' -o likes
```

Finally, I wanted to commit and push the data. This yielded an error in
cases, where there was no change to the data. I found the following
helpful command:

``` sh
git diff-index --quiet HEAD || (git commit -a -m'[bot]: update latest twitter likes data' --allow-empty && git push -f)`
```

Apparently we can just use Boolean operators. I’m not exactly sure how
the single elements evaluate, but it seems like: Either there is no
difference between the latest commit and the working tree or commit and
push.

After figuring out how to run the job, I scheduled it to be executed
every 12 hours: `'0 */12 * * *'`

The whole YAML script can be found
[here](https://github.com/TimTeaFan/loopurrr/blob/main/.github/workflows/like-badge.yaml).

#### 4. shields.io

The final step was to create a dynamic [shields.io](https://shields.io)
badge. Here the only tricky part was the “query” specification. It
helped to transform the json file from the Twitter API into an R list
object and subset it to get the likes:

`data[[1]]$public_metrics$like_count` in R corresponds to
`.data..public_metrics.like_count`.

This is the full URL:

`https://img․shields․io/badge/dynamic/json?url=https://raw․githubusercontent․com/TimTeaFan/loopurrr/twitter-like-badge/likes&label=Likes&query=$.data..public_metrics.like_count&style=social&logo=Twitter`

We can just add this URL to the source attribute of `<img src = …>` in
the README.Rmd and the badge will be rendered on Github as well as on
the pkgdown website.
