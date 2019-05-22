#!/usr/bin/env ruby

require 'json'

REPOS = {
  '10979413': { # rir_mirror
    repos: [
      560429,  # /
      560812   # /benchmarks
    ],
    keep_recent: 3,
    keep: [
      'master',
      'c13be9ca843726e3eb3e57e6e6f2a602ae7481eb'
    ]},
  '12325205': {# rir experiments
    repos: [
      562769,  # scope_resolution
    ],
    keep_recent: 1,
    keep: [
      'c13be9ca843726e3eb3e57e6e6f2a602ae7481eb-210646f2a98099ed48d928b1f5e66e551aa0d92b',
    ]},
}

TOKEN = ARGF.read

def curl(what)
  JSON.parse(`curl -s --header "PRIVATE-TOKEN: #{TOKEN}" #{what}`)
end

def fetch(project, repo, what)
  curl("https://gitlab.com/api/v4/projects/#{project}/registry/repositories/#{repo}/#{what}")
end

def delete(project, repo, what)
  curl("--request DELETE https://gitlab.com/api/v4/projects/#{project}/registry/repositories/#{repo}/#{what}")
end

SKIP=3

REPOS.each do |project, repos|
  repos[:repos].each do |repo|
    puts "== #{project} == #{repo} =="
    res = fetch(project, repo, "tags")
    i = 0
    res.each do |tag|
      if i < repos[:keep_recent]
        puts "keep recent #{tag['name']}"
        i += 1
        next
      end
      unless repos[:keep].include? tag['name']
        puts "delete #{tag['name']}"
        puts delete(project, repo, "tags/#{tag['name']}")
      else
        puts "keep #{tag['name']}"
      end
    end
  end
end
