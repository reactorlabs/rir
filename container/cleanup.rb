#!/usr/bin/env ruby

require 'json'
require 'date'

REPOS = {
  '10979413': { # rir_mirror
    repos: [
      560429,  # /
      560812   # /benchmarks
    ],
    keep: [
      `git rev-parse HEAD`.chomp,                   # current version
      `git rev-parse HEAD~1`.chomp,                 # prev version
      'master',
      # dls 19 paper
      'dba88e9bc417325a29c91acb088df7fe8109ca39',
      # oopsla 20
      'bc1933dde2673bf830f4505bb2483cd1fdd282ab',
    ]},
  '12325205': {# rir experiments
    repos: [
      562769,  # scope_resolution
      576865,  # envs_created
    ],
    keep: [
      'dba88e9bc417325a29c91acb088df7fe8109ca39-e427e03931114e0715513bfafcd59a267812dcb1',
    ]},
}

TOKEN = ARGF.read.chomp

def curl(what)
  JSON.parse(`curl -s --header "PRIVATE-TOKEN: #{TOKEN}" #{what}`)
end

def fetch(project, repo, what)
  curl("https://gitlab.com/api/v4/projects/#{project}/registry/repositories/#{repo}/#{what}")
end

def delete(project, repo, what)
  curl("--request DELETE https://gitlab.com/api/v4/projects/#{project}/registry/repositories/#{repo}/#{what}")
end

MAX_AGE_DAYS=0.6

REPOS.each do |project, repos|
  repos[:repos].each do |repo|
    puts "== #{project} == #{repo} =="
    res = fetch(project, repo, "tags")
    res.each do |tag|
      if repos[:keep].include? tag['name']
        puts "keeping #{tag['name']} (whitelisted)"
      else
        info = fetch(project, repo, "tags/#{tag['name']}")
        t = DateTime.parse(info["created_at"])
        age = DateTime.now - t

        if age > MAX_AGE_DAYS
          puts "delete #{tag['name']} which is #{age.to_f}d old"
          puts delete(project, repo, "tags/#{tag['name']}")
        else
          puts "keeping #{tag['name']} (less than #{MAX_AGE_DAYS}d old)"
        end
      end
    end
  end
end
