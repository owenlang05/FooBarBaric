# Deliberate mutation: temporarily disable Bar-phase condition inversion.
# Performs following actions:
###1. Reads src/main/scala/foobarbaric/StatementInterpreter.scala and saves its original bytes in memory.
###2. Temporarily changes one line: case Phase.Bar => !value becomes case Phase.Bar => value.
###3. Runs existing FooBarOrdinaryTests suite and checks for the expected failure.
###4. Restores the original source bytes using a PowerShell finally block.
###5. Reruns the tests to verify the restored implementation passes.

# This script restores the EXACT original file bytes in a finally block.
# Run from the repository root: powershell -ExecutionPolicy Bypass -File .\scripts\run-mutation-test.ps1

$ErrorActionPreference = 'Continue'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$sourceFile = Join-Path $repo 'src\main\scala\foobarbaric\StatementInterpreter.scala'
$originalBytes = [System.IO.File]::ReadAllBytes($sourceFile)
$originalText = [System.IO.File]::ReadAllText($sourceFile)
$from = 'case Phase.Bar => !value'
$to = 'case Phase.Bar => value'

if (-not $originalText.Contains($from)) {
    throw "Expected original rule not found: $from. No file was changed."
}

$mutationDetected = $false
Push-Location $repo
try {
    Write-Host '=== TEMPORARY MUTATION: Bar conditions are NO LONGER inverted ==='
    [System.IO.File]::WriteAllText($sourceFile, $originalText.Replace($from, $to))
    $testOutput = (& sbt --no-server 'Test / testOnly foobarbaric.FooBarOrdinaryTests' 2>&1 | Out-String)
    Write-Host $testOutput
    $mutantExit = $LASTEXITCODE
    $mutationDetected = ($mutantExit -ne 0) -and
        ($testOutput -match 'Absolute value handles negative input in Bar phase.*FAILED')
} finally {
    [System.IO.File]::WriteAllBytes($sourceFile, $originalBytes)
    Write-Host '=== Original source restored (including original line endings) ==='
    Pop-Location
}

if (-not $mutationDetected) {
    throw 'Mutation test not established: check test log for targeted failure. Original file restored.'
}

Write-Host '=== Detected: changing Bar inversion caused the expected test to fail ==='
Push-Location $repo
try {
    & sbt --no-server 'Test / testOnly foobarbaric.FooBarOrdinaryTests'
    if ($LASTEXITCODE -ne 0) {
        throw 'Restored suite did not pass; inspect output.'
    }
    Write-Host '=== Restored original passes all ordinary-program tests ==='
} finally {
    Pop-Location
}
