# Windows launcher: start the viewer from this checkout on the project in the
# current directory. Mirrors scripts/uml; works in Windows PowerShell 5.1 and
# PowerShell 7.
param(
    [switch]$Restart,
    [switch]$Help,
    [Parameter(Position = 0)]
    [string]$Diagram
)

$ErrorActionPreference = 'Stop'

$checkout = (Split-Path -Parent $PSScriptRoot) -replace '\\', '/'
$quilVersion = '4.3.1563'
$logFile = 'uml-viewer-log.txt'

if ($Help) {
    Write-Output 'usage: uml.ps1 [-Restart] [-Help] [diagram.edn]'
    Write-Output '  Run from the project you want to view.'
    Write-Output '  -Restart    open the diagram without a companion (use this on Windows).'
    Write-Output '              With no diagram.edn, examples/<project>.edn or the first'
    Write-Output '              examples/*.edn that is not a policy.'
    Write-Output '  The viewer detaches; its output is appended to uml-viewer-log.txt.'
    exit 0
}

function Find-Diagram {
    $projectName = Split-Path -Leaf (Get-Location).ProviderPath
    $named = Join-Path 'examples' "$projectName.edn"
    if (Test-Path -LiteralPath $named -PathType Leaf) { return $named }
    if (Test-Path -LiteralPath 'examples' -PathType Container) {
        $first = Get-ChildItem -LiteralPath 'examples' -Filter '*.edn' -File |
            Where-Object { $_.Name -notlike '*.policy.edn' } |
            Sort-Object Name |
            Select-Object -First 1
        if ($first) { return (Join-Path 'examples' $first.Name) }
    }
}

$viewerArguments = @()
if ($Restart) { $viewerArguments += '--restart' }
if (-not $Diagram -and $Restart) { $Diagram = Find-Diagram }
if ($Diagram) { $viewerArguments += $Diagram }

# \" is how a double quote survives the Windows command line
$dependencies = '{:deps {uml-viewer/uml-viewer {:local/root \"' + $checkout + '\"} quil/quil {:mvn/version \"' + $quilVersion + '\"}}}'
$command = 'clojure -Sdeps "' + $dependencies + '" -M -m uml-viewer.main.uml-viewer'
foreach ($argument in $viewerArguments) { $command += ' "' + $argument + '"' }

$startedAt = Get-Date -Format 'yyyy-MM-dd HH:mm:ss'
Add-Content -LiteralPath $logFile -Value ("----- $startedAt starting uml-viewer " + ($viewerArguments -join ' '))

# cmd.exe appends stdout and stderr to the log; Start-Process can only overwrite a file.
$process = Start-Process -FilePath 'cmd.exe' `
    -ArgumentList ('/d /s /c "' + $command + ' >> ' + $logFile + ' 2>&1"') `
    -WorkingDirectory (Get-Location).ProviderPath `
    -WindowStyle Hidden `
    -PassThru
Write-Output "UML viewer started (pid $($process.Id)). Log: $logFile"
