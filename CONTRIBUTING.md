# Contributing to ARWpost CSIR CHPC

## How to Contribute

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test on the Lengau cluster
5. Submit a pull request

## Testing Guidelines

- Test all scripts on the cluster
- Verify module loading works
- Check PBS job submission
- Ensure compatibility with Intel compilers

## Code Style

- Use consistent bash scripting style
- Add comments to complex sections
- Test scripts before committing

## Cluster Testing

Before submitting changes, please test on the Lengau cluster:

```bash
# Test module loading
module load arwpost/3.1

# Test PBS job submission
qsub examples/arwpost_job.pbs

# Verify installation scripts
./scripts/install_arwpost_minimal_final.sh
```
