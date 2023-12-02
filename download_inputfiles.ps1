$URI = "https://adventofcode.com"
Start-Sleep -Milliseconds 1  # Needed for https://stackoverflow.com/a/49859001/180174
$webSession = [Microsoft.PowerShell.Commands.WebRequestSession]::new()
$webSession.Cookies.Add($URI,[System.Net.Cookie]::new("session", $Env:AOC_SESSION_COOKIE))
$DATE = Get-Date
for ($i = 1; $i -le $DATE.Day; $i++) {
    $fileUrl = "https://adventofcode.com/$( $DATE.Year )/day/$i/input"
    Write-Host "Downloading $fileUrl"
    try
    {
        Invoke-Webrequest $fileUrl -WebSession $webSession -OutFile input/$i.txt | Out-Null
    }
    catch
    {
        break
    }
}

