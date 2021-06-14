#!/usr/bin/env ruby

require 'yaml'

GH_URL = "https://api.github.com/repos/reactorlabs/rir"
GL_URL = "https://gitlab.com/api/v4/projects/10979413"
SP_URL = "https://rir-benchmarks.prl.fit.cvut.cz"

GH_TOKEN = ENV['GH_TOKEN']
sha = ENV['CI_COMMIT_SHA']

def get_pr(q)
  q = q.downcase
  prs = YAML.load(`curl -s #{GH_URL}/pulls?state=open`)
  prs.each do |pr|
    sha = pr['head']['sha'].downcase
    if sha == q
      return {
        id: pr['id'],
        url: pr['issue_url'],
        base: pr['base']['sha'].downcase
      }
    end
  end
  nil
end

def get_pipeline(q)
  comm = YAML.load(`curl -s #{GL_URL}/repository/commits/#{q}`)
  pl_id = comm['last_pipeline']['id']
  YAML.load(`curl -s #{GL_URL}/pipelines/#{pl_id}/jobs`)
end

pr = get_pr(sha)
unless pr
  puts "This commit has no associated PR"
  exit 0
end

head_jobs = get_pipeline(sha)
max_job = head_jobs.map{|j| [j['duration'], j['name']]}.sort{|a,b| a[0] <=> b[0]}.last

head_bm = head_jobs.select{|j| j['name'] == 'benchmark_llvm' && j['status'] == 'success'}.first
unless head_bm
  puts "Unfortunately I could not find a successufl benchmark job for this commit"
  exit 1
end
head_bm = head_bm['id']
base_bm = get_pipeline(pr[:base]).select{|j| j['name'] == 'benchmark_llvm' && j['status'] == 'success'}.first
unless base_bm
  puts "Unfortunately I could not find a successufl benchmark job for the baseline"
  puts "Either I github reported the wrong baseline (#{pr[:base]}) or the baseline does not have a successful benchmark run."
  exit 1
end
base_bm = base_bm['id']

diff_url = "#{SP_URL}/diff?job_ids[]=#{head_bm}&job_ids[]=#{base_bm}&selection=all"
diff_yaml_url = diff_url.gsub("diff", "diff.yaml")

puts "Fetching #{diff_yaml_url}"

res = `curl -L -s "#{diff_yaml_url}"`
res = YAML.load(res)

big_change = res[0][:diff].map{|r| ["#{r[0]} suite", r[1][:mean]]}.select{|(k,v)| k != 'summary' && (v > 1.02 || v < 0.98)} +
  res[0][:diff].map{|r| r[1][:res]}.flatten.each_slice(2).to_a.map{|a| [a[0], a[1][:mean]]}.select{|a| a[1] < 0.95}

summary = res[0][:diff].map{|r| [r[0], r[1][:mean]]}.select{|(k,_)| k == 'summary'}.first

text = "Here are some stats for your PR:\n\n"
text << "* "
if max_job[0].to_i > 2.2*60*50
  text << "WARNING: "
end
text << "the longest CI job #{max_job[1]} took #{(max_job[0] / 60.0 / 60.0).round(2)}h\n"

text << big_change.map{|(k,v)| "* #{k} #{if v>1.0 then 'improved' else 'regressed' end} by #{v.round(2)}\n"}.join
text << "* Overall benchmarks #{if summary[1]>1.0 then 'improved' else 'regressed' end} by #{summary[1].round(2)}\n"

text << "\nPlease find your performance results at #{diff_url}\n"

puts text

puts `curl -s -H "Authorization: token #{GH_TOKEN}" \
 -X POST -d '{"body": "#{text.split("\n").join('\n')}"}' \
 "#{pr[:url]}/comments"`
